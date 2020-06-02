#!/usr/bin/env python
from __future__ import unicode_literals, print_function

# MIT License
#
# Copyright (c) 2019 Colin McMillen
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

"""makes an ASCII-art comic in the style of webcomicname.com."""

import re
import sys


USAGE = """Usage:
$ ohnosay.py "you can make a single panel of text"
$ ohnosay.py "you can also make two panels" "but three isn't supported"
"""


TEMPLATE = r'''
  /--------------------------\  |
  | aaaaaaaaaaaaaaaaaaaaaaaa |  |
  \_ ________________________/  |
    v                           |
     _____                      |
    / . . \                     |
  \ |  _  | /                   |
   v       v                    |'''

TO_REPLACE = 'aaaaaaaaaaaaaaaaaaaaaaaa'

OH_NO = r'''
  /------\
  | ohno |
  \_ ____/
    v
     _____
    / . . \
  _ |  _  | _
   v       v'''


def chunk_lines(s):
  """Splits a string into a list of lines that fit inside the speech bubble."""
  if len(s) <= len(TO_REPLACE):
    return [s]
  try:
    split_position = s[:len(TO_REPLACE) + 1].rindex(' ')
  except ValueError:  # some word is too big to fit! oh no
    split_position = len(TO_REPLACE)
  return [s[:split_position]] + chunk_lines(s[split_position + 1:])


def main(args):
  """does some things. oh no"""
  if not args:
    print(USAGE)
    return

  single_panel = False
  if len(args) == 1:
    single_panel = True
    args.append('')

  lines_1 = chunk_lines(args[0])
  lines_2 = chunk_lines(args[1])

  # the user input has to be at least two lines tall, because the final 'oh no'
  # panel is two lines tall and cutting out the second line would be... oh no
  if len(lines_1) == 1:
    lines_1.append('')

  # incredibly cheesy vertical centering
  while len(lines_1) < len(lines_2):
    if len(lines_1) == len(lines_2) - 1:
      lines_1.append('')
    else:
      lines_1 = [''] + lines_1 + ['']
  while len(lines_2) < len(lines_1):
    if len(lines_2) == len(lines_1) - 1:
      lines_2.append('')
    else:
      lines_2 = [''] + lines_2 + ['']

  # now both lines are the same length
  num_lines = len(lines_1)

  comic = re.sub('-', '\u203e', TEMPLATE).split('\n')
  oh_no = re.sub('-', '\u203e', OH_NO).split('\n')

  # this got hacky and gross. oh no
  for i, _ in enumerate(comic):
    if i == 2:
      for j in range(num_lines):
        left_centered = lines_1[j].center(len(TO_REPLACE))
        left = re.sub(TO_REPLACE, left_centered, comic[2])
        middle_centered = lines_2[j].center(len(TO_REPLACE))
        middle = re.sub(TO_REPLACE, middle_centered, comic[2])
        if single_panel:
          middle = ''
        if j == num_lines - 3:
          print(left + middle + oh_no[1])
        elif j == num_lines - 2:
          print(left + middle + '  | oh   |')
        elif j == num_lines - 1:
          print(left + middle + '  |   no |')
        else:
          print(left + middle)
    else:
      oh_no_line = oh_no[i]
      if num_lines > 2 and i == 1:
        oh_no_line = ''
      if single_panel:
        print(comic[i] + oh_no_line)
      else:
        print(comic[i] + comic[i] + oh_no_line)


if __name__ == '__main__':
  main(sys.argv[1:])
