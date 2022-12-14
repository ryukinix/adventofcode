// Copyright 2022 to ryukinix. All rights reserved
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package lerax

import (
	"reflect"
	"testing"
)

func TestIntersectionEmptyCase(t *testing.T) {
	got := Intersection("aaa", "bbb")
	expected := ""
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestIntersectionMixupChars(t *testing.T) {
	got := Intersection("afds", "lpfa")
	expected := "af"
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestIntersectionFullCase(t *testing.T) {
	got := Intersection("fdsa", "asdf")
	expected := "adfs"
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestIntersectionStringsDifferentSize(t *testing.T) {
	got := Intersection("ab", "rewqfa")
	expected := "a"
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestIntersectionUniqueResult(t *testing.T) {
	//t.Skip("I'm done with this, my intersection function is a piece of shit")
	got := Intersection("ZTmtZvZLTFNLMQMNRvZ", "ncdcHwcScJvcdHnVfwV")
	expected := "v"
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}

	got = Intersection("bxbzb", "bcbab")
	expected = "b"
	if got != expected {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestUniqueChars(t *testing.T) {
	got := UniqueChars("asdf")
	expected := true
	if got != expected {
		t.Errorf("Expected %v, got: %v", expected, got)
	}

	got = UniqueChars("aabb")
	expected = false
	if got != expected {
		t.Errorf("Expected %v, got: %v", expected, got)
	}
}

func TestSplitInTheMiddleBasic(t *testing.T) {
	got1, got2 := SplitInTheMiddle("asdfasdf")
	if got1 != got2 {
		t.Errorf("Expected to be equal, got: %q and %q", got1, got2)
	}
}

func TestGroupLinesByWindow(t *testing.T) {
	got := GroupLinesByWindow([]string{"a", "b", "c", "d"}, 2)
	expected := [][]string{{"a", "b"}, {"c", "d"}}
	if !reflect.DeepEqual(got, expected) {
		t.Errorf("Expected %q, got: %q", expected, got)
	}
}

func TestReverse(t *testing.T) {
	test := Reverse([]int{1, 2, 3})
	expected := []int{3, 2, 1}
	if !reflect.DeepEqual(test, expected) {
		t.Errorf("Expected %q, got %q", expected, test)
	}
}
