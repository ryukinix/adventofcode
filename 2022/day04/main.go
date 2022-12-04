// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"strings"

	"github./ryukinix/adventofcode/lerax"
)

type fnOverlaps func(lerax.Range, lerax.Range) bool

func resolve(pairs []string, overlaps fnOverlaps) int {
	overlapping := 0
	for _, pair := range pairs {
		assignments := strings.Split(pair, ",")
		r1 := lerax.ParseRange(assignments[0])
		r2 := lerax.ParseRange(assignments[1])
		if overlaps(r1, r2) {
			overlapping++
		}
	}
	return overlapping
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer func() {
		lerax.ErrCheck(readFile.Close())
	}()

	pairs := lerax.LoadLines(readFile)
	// part1
	fmt.Println(resolve(pairs, lerax.Range.Overlaps))
	// part2
	fmt.Println(resolve(pairs, lerax.Range.OverlapsPartial))
}
