// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"sort"

	"github.com/ryukinix/adventofcode/lerax"
)

func part1(calories []int) {
	fmt.Println(lerax.MaxArray(calories))
}

func part2(calories []int) {
	sort.Slice(calories, func(a, b int) bool {
		return calories[a] > calories[b]
	})
	result := calories[0] + calories[1] + calories[2]
	fmt.Println(result)
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer func() {
		lerax.ErrCheck(readFile.Close())
	}()

	calories := lerax.CalculateTotalOfEachGroup(lerax.LoadLinesGrouped(readFile))
	part1(calories)
	part2(calories)
}
