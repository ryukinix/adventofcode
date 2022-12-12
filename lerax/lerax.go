// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lerax

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"

	"golang.org/x/exp/constraints"
)

type Number interface {
	constraints.Float | constraints.Integer
}

// ErrCheck check if is not nil and print it
// otherwise, does nothing.
func ErrCheck(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
}

// Reverse any slice producing a new one.
//
// ... why go doesn't have that builtin?
func Reverse[S ~[]E, E any](slice S) []E {
	s := make([]E, len(slice))
	copy(s, slice)
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
	return s
}

// MaxArray calculate the max value of atn array (slice).
func MaxArray[T constraints.Ordered](array []T) T {
	var m T
	for i, e := range array {
		if i == 0 || e > m {
			m = e
		}
	}
	return m
}

// SumArray sum the values of an array.
func SumArray[N Number](array []N) N {
	var sum N = 0
	for _, e := range array {
		sum = sum + e
	}
	return sum
}

// GroupLinesByWindow create successive groups by window.
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

func ReadFileText(inputFile string) string {
	file, err := ioutil.ReadFile(inputFile)
	if err != nil {
		fmt.Printf("Could not read the file due to this %s error \n", err)
	}
	// convert the file binary into a string using string
	fileContent := string(file)
	return fileContent
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

// SplitInTheMiddle splits a string in two equal parts.
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

// Intersection returns the common elements of two strings.
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

// UniqueChars verify if the string s doesn't have any char repeated.
func UniqueChars(s string) bool {
	return len(s) == len(Intersection(s, s))
}

func IsInt(s string) bool {
	if _, err := strconv.Atoi(s); err == nil {
		return true
	}
	return false
}
