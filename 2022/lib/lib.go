// Copyright 2022 the adventofcode Authors. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lib

import (
	"bufio"
	"os"
	"strconv"

	"github.com/ryukinix/adventofcode/lerax"
)

func LoadCaloriesByElf(filePath string) [][]int {
	readFile, err := os.Open(filePath)
	lerax.ErrCheck(err)
	defer func() {
		lerax.ErrCheck(readFile.Close())
	}()

	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)
	var fileLines [][]int

	lineIndex := 0
	fileLines = append(fileLines, []int{})
	for fileScanner.Scan() {
		line := fileScanner.Text()
		if line == "" {
			lineIndex++
			fileLines = append(fileLines, []int{})
			continue
		}
		calory, err := strconv.Atoi(line)
		lerax.ErrCheck(err)
		fileLines[lineIndex] = append(fileLines[lineIndex], calory)
	}

	return fileLines
}

func CalculateTotalCaloriesByElf(caloriesByElf [][]int) []int {
	var result []int
	for _, calories := range caloriesByElf {
		result = append(result, lerax.SumArray(calories))
	}
	return result
}
