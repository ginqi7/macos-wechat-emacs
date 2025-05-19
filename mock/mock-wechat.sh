#!/bin/bash

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
json_file="${script_dir}/$1.json"
cat "$json_file"
