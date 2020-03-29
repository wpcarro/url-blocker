# url-blocker

`url-blocker` blocks the URLs that you want to block when you want it to block
them.

Let's say that you don't want to visit Twitter during the work week. Create the
file `/etc/url-blocker/rules.json` with the following contents and
`url-blocker` will take care of the rest.

```json
# /etc/url-blocker/rules.json
[
  {
    "urls": [
      "twitter.com",
      "www.twitter.com",
    ],
    "allowed": [
      {
        "day": "Saturday",
        "timeslots": [
          "00:00-11:59"
        ]
      },
      {
        "day": "Sunday",
        "timeslots": [
          "00:00-11:59"
        ]
      }
    ]
  }
]
```

## Installation

This project builds with the latest version of `nixpkgs-unstable`,
(i.e. `https://github.com/NixOS/nixpkgs-channels` at commit
`ae6bdcc53584aaf20211ce1814bea97ece08a248`).

```shell
$ nix-build .
```

## How does it work?

`systemd` is intended to run `url-blocker` once every minute. `url-blocker` will
read `/etc/hosts` and map the URLs defined in `rules.json` to `127.0.0.1` when
you want them blocked. Because `systemd` runs once every minute, `/etc/hosts`
should be current to the minute as well.

I have not setup the `systemd` units, but I encourage anyone who may be
interested in this to create a pull request.

## Shortcomings

I would like to move the `/etc/url-blocker/rules.json` file to
`~/.config/url-blocker/rules.json`. I currently do not know if it is possible to
define user-specific rules. If Linux permits this, `systemd` can run this as a
`--user` module, which I would prefer.

I read that some people have successfully created user-specific rules by setting
the `HOSTALIAS` environment variable to some file (e.g. `~/hosts`). I could not
get this to work on my end, but if other people can successfully use this, I
would prefer supporting that instead.
