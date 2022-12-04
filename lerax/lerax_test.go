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
