// Copyright 2022 the adventofcode Authors. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"

	lib "github.com/ryukinix/adventofcode/2022/lib"
	"github.com/ryukinix/adventofcode/lerax"
)

func main() {
	calories := lib.CalculateTotalCaloriesByElf(lib.LoadCaloriesByElf("input.txt"))
	fmt.Println(lerax.MaxArray(calories))
}
