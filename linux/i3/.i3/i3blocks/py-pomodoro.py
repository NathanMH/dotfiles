#!/bin/python3

import time

STATUS_FILE = "/tmp/pomodoro_status"

def read_status_file(status_file):
    STATUS = status_file.readline()
    REMAINING = status_file.readline()
    return STATUS, REMAINING

# with open(STATUS_FILE, "w+") as f:
#     STATUS, REMAINING = read_status_file(f)

#     print("test")
#     if not STATUS:

print('test')
