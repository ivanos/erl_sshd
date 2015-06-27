Put the user's authorized_keys file here.

To create a new user key:

```
ssh-keygen -t rsa -f id_rsa
cp id_rsa.pub authorized_keys
```

Then use the id_rsa as your private key, for example:

```
mkdir ~/.flare_ssh
chmod 700 ~/.flare_ssh
cp id_rsa id_rsa.pub ~/.flare_ssh
chmod 600 ~/.flare_ssh/id_rsa
ssh hostname -p 11111 -i ~/.flare_ssh/id_rsa -o UserKnownHostsFile=~/.flare_ssh/known_hosts
```

Alternatively you can put your own key in the authorized_keys file and use
that key to connect.
