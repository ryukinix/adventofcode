package lerax

import (
	"fmt"
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
