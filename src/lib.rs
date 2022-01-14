mod canonical;
pub mod json;
#[cfg(feature = "signed")]
pub mod signed;

use anyhow::Error;
pub use canonical::Canonical;
#[doc(inline)]
pub use json::{to_string_canonical, to_vec_canonical};
#[doc(inline)]
#[cfg(feature = "signed")]
pub use signed::Signed;

pub use ed25519_dalek::{Keypair, PublicKey, SecretKey};

/// Create a [`Keypair`] from a serialized [`SecretKey`].
pub fn keypair_from_secret_bytes(bytes: &[u8]) -> Result<Keypair, Error> {
    let secret = SecretKey::from_bytes(bytes)?;
    let public = PublicKey::from(&secret);

    Ok(Keypair { public, secret })
}
