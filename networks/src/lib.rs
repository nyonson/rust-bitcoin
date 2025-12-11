// SPDX-License-Identifier: CC0-1.0

//! Bitcoin network types.
//!
//! This crate provides types for identifying which Bitcoin network (chain) is being used.
//! The term "network" is overloaded, here [`Network`] refers to the specific
//! Bitcoin network we are operating on e.g., mainnet, testnet, signet, regtest.
//! The terms "network" and "chain" are often used interchangeably for this concept.

#![no_std]
// Coding conventions.
#![warn(missing_docs)]
#![warn(deprecated_in_future)]
#![doc(test(attr(warn(unused))))]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

use core::fmt;
use core::str::FromStr;

use alloc::borrow::ToOwned;
use alloc::string::String;

use internals::write_err;

#[cfg(feature = "serde")]
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

/// What kind of network we are on.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NetworkKind {
    /// The Bitcoin mainnet network.
    Main,
    /// Some kind of testnet network.
    Test,
}

// We explicitly do not provide `is_testnet`, using `!network.is_mainnet()` is less
// ambiguous due to confusion caused by signet/testnet/regtest.
impl NetworkKind {
    /// Returns true if this is real mainnet bitcoin.
    pub fn is_mainnet(&self) -> bool { *self == Self::Main }
}

impl From<Network> for NetworkKind {
    fn from(n: Network) -> Self {
        use Network::*;

        match n {
            Bitcoin => Self::Main,
            Testnet(_) | Signet | Regtest => Self::Test,
        }
    }
}

/// The testnet version to act on.
#[derive(Copy, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
#[non_exhaustive]
pub enum TestnetVersion {
    /// Testnet version 3.
    V3,
    /// Testnet version 4.
    V4,
}

/// The cryptocurrency network to act on.
///
/// This is an exhaustive enum, meaning that we cannot add any future networks without defining a
/// new, incompatible version of this type. If you are using this type directly and wish to support the
/// new network, this will be a breaking change to your APIs and likely require changes in your code.
#[derive(Copy, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Network {
    /// Mainnet Bitcoin.
    Bitcoin,
    /// Bitcoin's testnet network.
    Testnet(TestnetVersion),
    /// Bitcoin's signet network.
    Signet,
    /// Bitcoin's regtest network.
    Regtest,
}

#[cfg(feature = "serde")]
impl Serialize for Network {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_display_str())
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Network {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct NetworkVisitor;

        impl Visitor<'_> for NetworkVisitor {
            type Value = Network;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a valid network identifier")
            }

            fn visit_str<E>(self, value: &str) -> Result<Network, E>
            where
                E: serde::de::Error,
            {
                Network::from_str(value).map_err(E::custom)
            }
        }

        deserializer.deserialize_str(NetworkVisitor)
    }
}

impl Network {
    /// Converts a `Network` to its equivalent `bitcoind -chain` argument name.
    ///
    /// ```bash
    /// $ bitcoin-23.0/bin/bitcoind --help | grep -C 3 '\-chain=<chain>'
    /// Chain selection options:
    ///
    /// -chain=<chain>
    /// Use the chain <chain> (default: main). Allowed values: main, test, signet, regtest
    /// ```
    pub fn to_core_arg(self) -> &'static str {
        match self {
            Self::Bitcoin => "main",
            // For user-side compatibility, testnet3 is retained as test
            Self::Testnet(TestnetVersion::V3) => "test",
            Self::Testnet(TestnetVersion::V4) => "testnet4",
            Self::Signet => "signet",
            Self::Regtest => "regtest",
        }
    }

    /// Converts a `bitcoind -chain` argument name to its equivalent `Network`.
    ///
    /// ```bash
    /// $ bitcoin-23.0/bin/bitcoind --help | grep -C 3 '\-chain=<chain>'
    /// Chain selection options:
    ///
    /// -chain=<chain>
    /// Use the chain <chain> (default: main). Allowed values: main, test, signet, regtest
    /// ```
    pub fn from_core_arg(core_arg: &str) -> Result<Self, ParseNetworkError> {
        use Network::*;

        let network = match core_arg {
            "main" => Bitcoin,
            "test" => Testnet(TestnetVersion::V3),
            "testnet4" => Testnet(TestnetVersion::V4),
            "signet" => Signet,
            "regtest" => Regtest,
            _ => return Err(ParseNetworkError(core_arg.to_owned())),
        };
        Ok(network)
    }

    /// Returns a string representation of the `Network` enum variant.
    /// This is useful for displaying the network type as a string.
    const fn as_display_str(self) -> &'static str {
        match self {
            Self::Bitcoin => "bitcoin",
            Self::Testnet(TestnetVersion::V3) => "testnet",
            Self::Testnet(TestnetVersion::V4) => "testnet4",
            Self::Signet => "signet",
            Self::Regtest => "regtest",
        }
    }
}

#[cfg(feature = "serde")]
pub mod as_core_arg {
    //! Module for serialization/deserialization of network variants into/from Bitcoin Core values
    #![allow(missing_docs)]

    use crate::Network;

    pub fn serialize<S>(network: &Network, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(network.to_core_arg())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Network, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct NetworkVisitor;

        impl serde::de::Visitor<'_> for NetworkVisitor {
            type Value = Network;

            fn visit_str<E: serde::de::Error>(self, s: &str) -> Result<Self::Value, E> {
                Network::from_core_arg(s).map_err(|_| {
                    E::invalid_value(
                        serde::de::Unexpected::Str(s),
                        &"bitcoin network encoded as a string (either main, test, testnet4, signet or regtest)",
                    )
                })
            }

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(
                    formatter,
                    "bitcoin network encoded as a string (either main, test, testnet4, signet or regtest)"
                )
            }
        }

        deserializer.deserialize_str(NetworkVisitor)
    }
}

/// An error in parsing network string.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct ParseNetworkError(String);

impl fmt::Display for ParseNetworkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write_err!(f, "failed to parse {} as network", self.0; self)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseNetworkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> { None }
}

impl FromStr for Network {
    type Err = ParseNetworkError;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bitcoin" => Ok(Self::Bitcoin),
            // For user-side compatibility, testnet3 is retained as testnet
            "testnet" => Ok(Self::Testnet(TestnetVersion::V3)),
            "testnet4" => Ok(Self::Testnet(TestnetVersion::V4)),
            "signet" => Ok(Self::Signet),
            "regtest" => Ok(Self::Regtest),
            _ => Err(ParseNetworkError(s.to_owned())),
        }
    }
}

impl fmt::Display for Network {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.as_display_str())
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use super::{Network, TestnetVersion};

    #[test]
    fn string() {
        assert_eq!(Network::Bitcoin.to_string(), "bitcoin");
        assert_eq!(Network::Testnet(TestnetVersion::V3).to_string(), "testnet");
        assert_eq!(Network::Testnet(TestnetVersion::V4).to_string(), "testnet4");
        assert_eq!(Network::Regtest.to_string(), "regtest");
        assert_eq!(Network::Signet.to_string(), "signet");

        assert_eq!("bitcoin".parse::<Network>().unwrap(), Network::Bitcoin);
        assert_eq!("testnet".parse::<Network>().unwrap(), Network::Testnet(TestnetVersion::V3));
        assert_eq!("testnet4".parse::<Network>().unwrap(), Network::Testnet(TestnetVersion::V4));
        assert_eq!("regtest".parse::<Network>().unwrap(), Network::Regtest);
        assert_eq!("signet".parse::<Network>().unwrap(), Network::Signet);
        assert!("fakenet".parse::<Network>().is_err());
    }

    #[test]
    fn from_to_core_arg() {
        let expected_pairs = [
            (Network::Bitcoin, "main"),
            (Network::Testnet(TestnetVersion::V3), "test"),
            (Network::Testnet(TestnetVersion::V4), "testnet4"),
            (Network::Regtest, "regtest"),
            (Network::Signet, "signet"),
        ];

        for (net, core_arg) in &expected_pairs {
            assert_eq!(Network::from_core_arg(core_arg), Ok(*net));
            assert_eq!(net.to_core_arg(), *core_arg);
        }
    }
}
