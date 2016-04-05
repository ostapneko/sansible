# Overview

**This is deprecated in favour of the [Scala version](https://github.com/citycontext/sansible) which provides an exhaustive modeling of Ansible's modules.**

Sansible provides a way to transpile Haskell code into YAML file suitable to be used as Ansible playbooks.

The reasons to do that are:

  - providing a safety net while writing playbooks, especially with regards to variable scoping and permissible module parameters.
  - providing freedom of abstraction, particularily with respect to composability, by dealing with mostly declarative code instead of data with some programmatic bits and pieces.

A primitive but useful encryption mechanism is also provided to protect your secrets a la Ansible Vault.

This is (and will stay) super hacky. Don't try this at home.

# Usage

## Writing playbooks

Write your playbooks in Haskell. See `src/Examples/Example.hs`.

## Writing modules

See the `src/AnsibleModules`. One thing to note: if you use an ADT with nullary constructors to express a finite set of possible choices (like in `src/AnsibleModules/File.hs`), and derive the `ToJSON` instance like in the examples, the constructor names will be snake cased. To avoid conflicts with other constructor, you can prepend them by `Choice`. For example `FileContent` and `ChoiceFileContent` will both be transformed to `file_content`.

## Encryption

Sansible comes with a trivial executable called `sansible-vault` to allow you to encrypt/decrypt the content of a folder called `clear_secrets` in the current working directory to a `secrets` folder.

A typical use case would be to:

  1. Include `clear_secrets` in the source dirs (`hs-source-dirs` if you use Cabal)
  2. Write some secret variables there using Haskell directly, to be used in your code
  3. Encrypt the `clear_secrets` with `sansible-vault encrypt -p path/to/password/file`
  4. Commit the encrypted `secrets` folder

When you checkout, you need to use `sansible-vault decrypt -p path/to/password/file`

WARNING: those command won't prompt you when overriding files out of sync in either way! They will only synchronize files with different decrypted content, though.
