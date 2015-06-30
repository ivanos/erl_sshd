# erl_sshd
Wrapper around Erlang ssh module to make it easy to add an ssh shell
to an Erlang node.  Add erl_sshd as rebar dependency and add
erl_sshd configuration to your release's sys.config file.

## Requirements
* Erlang 17+

## Using
Add erl_sshd as a dependency in your rebar.config file.  Then follow the
configuration instructions

## Configuration
erl_sshd is configured via its application environment variables. You
can set these in your release's `sys.config` file (see example).

The `port` environment variable is the listener port number.

The `app` environment variable is the name of the application that is
using erl_sshd as a dependency.  erl_sshd looks in the priv directory
of this application for the system key and authorized keys file.

You can use security keys and/or usernames and passwords to gain access to the
shell.

### Using keys
The host key and the `authorized_keys` file holding the user authorized
keys are in `priv/erl_sshd`.

You can generate keys with:

```sh
% deps/erl_sshd/makekeys
```

This creates a system key as `priv/erl_sshd/ssh_host_rsa_key`,
a public and private user key in the top directory, and a
authorized keys file as `priv/erl_sshd/authorized_keys`.  The
authorize keys file contains the generated public user key.

You may also copy your own keys into the `authorized_keys` file.

From the top level you can connect to your node using:

```sh
% ssh hostname -p 11122 -i id_rsa
```

### Using usernames and passwords
Follow the instructions above to create keys.  The username/password
authentication requires a system key to identify the host.

The `passwords` environment variable is a list of
two element tuples where the first element is the username and the
second element is the password.  These are strings.  For example:

```erlang
{passwords, [{"lincx","nbv123"}]}
```

allows the user `lincx` to connect with the password `nbv123`.

### Example

```erlang
[
  {erl_sshd, [
                {app, dobby},
                {port, 11122},
                {passwords, [{"lincx","nbv1234"},
                             {"bobs","youruncle"}]}
             ]},
  ... % environment variables for other applications in the node
].
```
