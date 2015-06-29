# erl_sshd
Wrapper around Erlang ssh module to make it easy to add an ssh shell
to an Erlang node.

## Requirements
* Erlang 17+

## Build
```sh
make
```

## Using
Add erl_sshd as a dependency in your rebar.config file.  Then follow the
configuration instructions

## Configuration
erl_sshd is configured via the application environment variables.  You
can set these in your node's sys.config file (see example).

The `port` application environment variable is the listener port number.

You can use keys and/or usernames and passwords to gain access to the
shell.

### Using keys
The host key is in `priv/system_dir` and the user authorized keys is in
`priv/user_dir`.  You can generate keys with:

```sh
make keys
```

This creates a system key as `priv/system_dir/ssh_host_rsa_key`,
a public and private user key in the top directory, and a
authorized keys file as `priv/user_dir/authorized_keys`.  The
authorize keys file contains the generated public user key.

You may also copy your own keys into the appropriate files.

See the README.md files in `priv/system_dir` and `priv/user_dir`.

### Using usernames and passwords
The `passwords` application environment variable key a list of
two element tuples where the first element is the username and the
second element is the password.  These are strings.  For example:

```erlang
{passwords, [{"lincx","nbv123"}]}
```

### Example

```erlang
[
  {erl_sshd, [
                {port, 11122},
                {passwords, [{"lincx","nbv1234"},
                             {"bobs","youruncle"}]}
             ]},
  ... % environment variables for other applications in the node
].
```
