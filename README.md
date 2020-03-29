# url-blocker

`url-blocker` blocks the URLs that you want to block when you want it to block
them.

Let's say that you don't want to visit Twitter during the work week. Create the
file `~/.config/url-blocker/rules.json` with the following contents and
`url-blocker` will take care of the rest.

```json
# ~/.config/url-blocker/rules.json
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

```shell
$ nix-env -iA 'briefcase.tools.url-blocker'
```

## How does it work?

`systemd` is intended to run `url-blocker` once every minute. `url-blocker` will
read `/etc/hosts` and map the URLs defined in `rules.json` to `127.0.0.1` when
you want them blocked. Because `systemd` run once every minute, `/etc/hosts`
should be current to the minute as well.
