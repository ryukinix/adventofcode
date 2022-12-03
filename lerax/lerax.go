package lerax

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// ErrCheck check if is not nil and print it.
// otherwise, does nothing.
func ErrCheck(err error) {
	if err != nil {
		fmt.Println(err)
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
