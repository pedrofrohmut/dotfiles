#! /usr/bin/env bash

volume_names=$(docker volume ls --format "{{.Name}}")

for name in $volume_names; do
    docker volume rm $name
done
