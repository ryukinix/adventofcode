// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"unicode"

	"github.com/ryukinix/adventofcode/lerax"
)

const upperRuneShift int = 38
const lowerRuneShift int = 96

func getPriority(r rune) int {
	var shift int = lowerRuneShift
	if unicode.IsUpper(r) {
		shift = upperRuneShift
	}
	return int(r) - shift
}

func resolvePart1(rucksacks []string) int {
	result := 0
	for _, rucksack := range rucksacks {
		fmt.Println("Input: ", rucksack)
		pack1, pack2 := lerax.SplitInTheMiddle(rucksack)
		fmt.Println("Pack1: ", pack1)
		fmt.Println("Pack2: ", pack2)
		repeated := lerax.Intersection(pack1, pack2)
		fmt.Println("Repeated: ", repeated)
		result = result + getPriority(rune(repeated[0]))
	}
	return result
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer func() {
		lerax.ErrCheck(readFile.Close())
	}()

	rucksacks := lerax.LoadLines(readFile)
	fmt.Println("Result part1: ", resolvePart1(rucksacks))
}
