// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lerax

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

func (r Range) Overlaps(r2 Range) bool {
	return r.Contains(r2) || r2.Contains(r)
}

// ErrCheck check if is not nil and print it.
// otherwise, does nothing.
func ErrCheck(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
}

// MaxArray calculate the max value of atn array (slice)
func MaxArray(array []int) int {
	var m int
	for i, e := range array {
		if i == 0 || e > m {
			m = e
		}
	}
	return m
}

// SumArray sum the values of an array
func SumArray(array []int) int {
	sum := 0
	for _, e := range array {
		sum = sum + e
	}
	return sum
}

// GroupLinesByWindow create successive groups by window
func GroupLinesByWindow(lines []string, window int) [][]string {
	var groups [][]string
	max := len(lines)
	for i := 0; i < max && i+window <= max; i = i + window {
		var group []string
		for j := i; j < i+window; j++ {
			group = append(group, lines[j])
		}
		groups = append(groups, group)
	}
	return groups
}

func LoadLinesGrouped(readFile *os.File) [][]string {
	var lineGroups [][]string = [][]string{{}}

	lineIndex := 0
	for _, line := range LoadLines(readFile) {
		if line == "" {
			lineIndex++
			lineGroups = append(lineGroups, []string{})
			continue
		}

		lineGroups[lineIndex] = append(lineGroups[lineIndex], line)
	}

	return lineGroups
}

func LoadLines(readFile *os.File) []string {
	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)
	var fileLines []string

	for fileScanner.Scan() {
		line := fileScanner.Text()
		fileLines = append(fileLines, line)
	}

	return fileLines
}

func CalculateTotalOfEachGroup(groupedLines [][]string) []int {
	var result []int
	for _, group := range groupedLines {
		var numbers []int
		for _, item := range group {
			item, err := strconv.Atoi(item)
			ErrCheck(err)
			numbers = append(numbers, item)
		}
		result = append(result, SumArray(numbers))
	}
	return result
}

// SplitInTheMiddle splits a string in two equal parts
//
// ex: SplitInTheMiddle("aaabbb") -> "aaa", "bbb"
func SplitInTheMiddle(s string) (string, string) {
	m := len(s) / 2
	s1 := s[:m]
	s2 := s[m:]
	return s1, s2
}

func SortString(s string) string {
	charArray := []rune(s)
	sort.Slice(charArray, func(i int, j int) bool {
		return charArray[i] < charArray[j]
	})
	return string(charArray)
}

// Intersection returns the common elements of two strings
//
// Complexity time: O(n * log(n)) (i think)
func Intersection(s1, s2 string) string {
	var result []rune
	var lastResult rune

	s1 = SortString(s1)
	s2 = SortString(s2)

	s2_pointer := 0
	for _, rune1 := range s1 {
		for s2_pointer < len(s2) {
			rune2 := rune(s2[s2_pointer])
			if rune1 == rune2 {
				// Don't append repeated Results
				if rune1 != lastResult {
					lastResult = rune1
					result = append(result, rune1)
				}
				s2_pointer++
				break
			} else if rune2 > rune1 {
				break // it's sorted, so there is no reason to check from here
			}
			s2_pointer++
		}
	}
	return string(result)
}
