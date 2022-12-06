// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lerax

import (
	"strconv"
	"strings"
)

type Range struct {
	min int
	max int
}

func NewRange(min, max int) Range {
	return Range{min: min, max: max}
}

// ParseRange parses string string like 1-10 to a Range struct
func ParseRange(s string) Range {
	minmax := strings.Split(s, "-")
	min, err1 := strconv.Atoi(minmax[0])
	max, err2 := strconv.Atoi(minmax[1])
	ErrCheck(err1)
	ErrCheck(err2)
	return NewRange(min, max)
}

func (r Range) Contains(r2 Range) bool {
	return r.min <= r2.min && r.max >= r2.max
}

// Overlaps check if two ranges overlaps totally in some way.
func (r Range) Overlaps(r2 Range) bool {
	return r.Contains(r2) || r2.Contains(r)
}

// OverlapsPartial check if two ranges overlaps at least partially.
func (r Range) OverlapsPartial(r2 Range) bool {
	return r.min <= r2.max && r2.min <= r.max
}
