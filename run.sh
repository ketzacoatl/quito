#!/bin/bash

set -uex

killall quito || true
quito &
