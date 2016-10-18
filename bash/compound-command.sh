#!/usr/bin/env bash

function parens () (
  foo=parens
  PATH=parens
  echo $PATH
)

function braces () {
  foo=braces
  PATH=braces
  echo $PATH
}

foo=bar
parens
echo PATH=$PATH
echo foo=$foo

foo=bar
braces
echo PATH=$PATH
echo foo=$foo
