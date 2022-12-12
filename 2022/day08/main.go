// Copyright 2022 to ryukinix All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/ryukinix/adventofcode/lerax"
)

// TreeContext models the up/down/left/right possible lines
// of visibility
type TreeContext struct {
	main           int
	fieldsOfVision [][]int
}

func (tc TreeContext) visible() bool {
	for _, field := range tc.fieldsOfVision {
		if tc.main > lerax.MaxArray(field) {
			return true
		}
	}
	return false
}

func boardColumn(matrix [][]int, columnIndex int) (column []int) {
	column = make([]int, 0)
	for _, row := range matrix {
		column = append(column, row[columnIndex])
	}
	return
}

func printMatrix(matrix [][]int) {
	for _, s := range matrix {
		for _, i := range s {
			fmt.Printf("%d", i)
		}
		println()
	}
}

func (t TreeContext) debug() {
	fmt.Printf("Main: %d \n", t.main)
	printMatrix(t.fieldsOfVision)
	fmt.Println("Visible: ", t.visible())
}

func (t TreeContext) scenicScore() int {
	var score int = 1
	for _, field := range t.fieldsOfVision {
		for j, tree := range field {
			if t.main <= tree || j == len(field)-1 {
				score = score * (j + 1)
				break
			}
		}
	}
	return score
}

func parseTrees(matrix [][]int) []TreeContext {
	m, n := len(matrix), len(matrix[0])
	// printMatrix(matrix)
	var trees []TreeContext
	for i := 1; i < m-1; i++ {
		for j := 1; j < n-1; j++ {
			main := matrix[i][j]
			if main == 0 {
				continue
			}
			column := boardColumn(matrix, j)
			left := matrix[i][j+1:]
			right := lerax.Reverse(matrix[i][:j])
			up := lerax.Reverse(column[:i])
			down := column[i+1:]
			t := TreeContext{
				main:           matrix[i][j],
				fieldsOfVision: [][]int{left, right, up, down},
			}
			// t.debug()
			trees = append(trees, t)
		}
	}
	return trees
}

func edgeTreesNumber(matrix [][]int) int {
	m, n := len(matrix)-1, len(matrix[0])-1
	return 2 * (m + n)
}

func fromStringsToMatrix(rows []string) [][]int {
	var matrix [][]int
	for _, row := range rows {
		var rowInt []int
		for _, digitStr := range row {
			digitInt, _ := strconv.Atoi(string(digitStr))
			rowInt = append(rowInt, digitInt)
		}
		matrix = append(matrix, rowInt)
	}
	return matrix
}

func resolvePart1(matrix [][]int) {
	tc := parseTrees(matrix)
	count := edgeTreesNumber(matrix)
	for _, t := range tc {
		if t.visible() {
			count = count + 1
		}
	}
	fmt.Println("Solution 1: ", count)
}

func resolvePart2(matrix [][]int) {
	tc := parseTrees(matrix)
	scores := make([]int, len(tc))
	for i := 0; i < len(scores); i++ {
		scores[i] = tc[i].scenicScore()
	}
	fmt.Println("Solution 2: ", lerax.MaxArray(scores))
}

func main() {
	readFile, err := os.Open("input.txt")
	lerax.ErrCheck(err)
	defer readFile.Close()

	matrix := fromStringsToMatrix(lerax.LoadLines(readFile))
	resolvePart1(matrix)
	resolvePart2(matrix)
}
