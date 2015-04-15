package main

import (
	"exmpl"
	"fmt"
	"reflect"
)

const (
	FirstType = iota * 7
	SecondType
	ThirdType
	FouthType
	FilthType
)

func main() {
	complex := complex(12, 7.6)
	var array1 [3]bool
	array2 := [...]int{1, -5, 56}
	array3 := [10]uint{5, 4, 3, 6, 7, 8}
	array4 := [3][3]float32{{3, 4.5, 5.4}, {3.5, 6, -67}}
	slice1 := []int{3, 4, 8, 43, 65, 43}
	slice2 := make([]byte, 5, 15)
	fmt.Println("Hello, world\n")
	fmt.Println("My OS core is - ", exmpl.GetOsType())
	fmt.Println("-----------------")
	fmt.Println("First ", FirstType)
	fmt.Println("Second ", SecondType)
	fmt.Println("Third ", ThirdType)
	fmt.Println("Fouth ", FouthType)
	fmt.Println("Filth ", FilthType)
	fmt.Println("-----------------")
	fmt.Printf("first (%s) - %v\n", reflect.TypeOf(complex), complex)
	fmt.Println("-----------------")
	fmt.Printf("array1 %d(%s) - %v\n", len(array1), reflect.TypeOf(array1), array1)
	fmt.Printf("array2 %d(%s) - %v\n", len(array2), reflect.TypeOf(array2), array2)
	fmt.Printf("array3 %d(%s) - %v\n", len(array3), reflect.TypeOf(array3), array3)
	fmt.Printf("array4 %d(%s) - %v\n", len(array4), reflect.TypeOf(array4), array4)
	fmt.Println("-----------------")
	fmt.Printf("slice1 %d(%s)%d - %v\n", len(slice1), reflect.TypeOf(slice1), cap(slice1), slice1)
	fmt.Printf("slice2 %d(%s)%d - %v\n", len(slice2), reflect.TypeOf(slice2), cap(slice2), slice2)
}
