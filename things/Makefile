# --- BEGIN AGPLv3-preamble ---
# Dactyl Marshmallow ergonomic keyboard generator
# Copyright (C) 2015, 2018 Matthew Adereth and Jared Jennings
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# --- END AGPLv3-preamble ---
SCADS = $(filter-out $(shell git ls-files), $(wildcard *.scad))
RIGHT_SCADS = $(filter $(SCADS), $(wildcard *-r*))
TARGETS = $(subst .scad,.stl,$(SCADS))
RIGHT_TARGETS = $(subst .scad,.stl,$(RIGHT_SCADS))



all: $(TARGETS)

right: $(RIGHT_TARGETS)

clean:
	rm -rf $(TARGETS)
realclean: clean
	rm -rf $(SCADS)
	rm -rf .deps

%.stl: %.scad
	openscad --render -o $@ $<

.PHONY: all clean realclean right
include .deps
