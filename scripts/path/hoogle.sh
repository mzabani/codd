#!/usr/bin/env bash

pkill -x hoogle || true
hoogle server --local --port=3000 2>/dev/null 1>/dev/null &
xdg-open http://localhost:3000/ 2>/dev/null 1>/dev/null &