#!/usr/bin/env bash

sudo ufw --force disable \
&& sudo ufw --force reset \
&& sudo ufw default deny incoming \
&& sudo ufw default allow outgoing \
&& sudo ufw allow transmission \
&& sudo ufw --force enable
