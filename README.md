# i3-analytics
Simple program which uses i3's IPC mechanism to record window focus events in a JSONL file and perform after the fact analysis on where all your time went.

TODOs:
* figure out how to detect when the screen is locked and filter those events out
    - should prob just watch processes for it...
* display percentages


## How to install
```
stack install
cp i3-analytics.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now i3-analytics
```

