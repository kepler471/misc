package main

import (
	"fmt"
)

type matrix [][]int

var count int

func main() {
	fmt.Print("\n\n\n")

	// Set problem
	A := [][]int{
		{1, 0, 0, 1, 0, 0, 1},
		{1, 0, 0, 1, 0, 0, 0},
		{0, 0, 0, 1, 1, 0, 1},
		{0, 0, 1, 0, 1, 1, 0},
		{0, 1, 1, 0, 0, 1, 1},
		{0, 1, 0, 0, 0, 0, 1},
	}

	vals := map[int]string{
		0: "a",
		1: "b",
		2: "c",
		3: "d",
		4: "e",
		5: "f",
	}

	var solution []string

	solution = solve(A, solution, vals)

	fmt.Println("Solution returned to main:", solution)

}

func solve(A matrix, solution []string, vals map[int]string) []string {

	count++
	fmt.Printf(
		"     _-_-_-_-_-_     \n"+
			"***  AlgoX call: %v  ***\n"+
			"     -_-_-_-_-_-     \n", count,
	)

	fmt.Println("Partial solution:", solution)

	// 1.
	//if len(A) == 0 || len(A[0]) == 0 { return solution }

	// 2.
	c := 0

	// 3.
	var initialRows []int // all rows where A<r,c> == 1, starting with c == 0
	for r := range A {
		if A[r][c] == 1 {
			initialRows = append(initialRows, r)
		}
	}
	fmt.Println("initial rows:", initialRows)

	for r := range initialRows {

		// 4.

		// 5.
		dr := make(map[int]bool)
		dc := make(map[int]bool)
		for j := 0; j < len(A[r]); j++ { // 0,1,2,3,4,5,6
			if !dc[j] {
				dc[j] = false
			}
			if A[r][j] == 1 { // 0,3,6
				for i := 0; i < len(A); i++ { // 0,1,2,3,4,5
					if !dr[i] {
						dr[i] = false
					}
					if A[i][j] == 1 { // (0,1),(0,1,2),(0,2,4,5)
						dr[i] = true
					}
				}
				dc[j] = true
			}
		}

		fmt.Println("dr:", dr)
		fmt.Println("dc:", dc)
		var B matrix
		shift := make(map[int]string)

		for ri := 0; ri < len(dr); ri++ {
			if !dr[ri] {
				var row []int
				for ci := 0; ci < len(dc); ci++ {
					if !dc[ci] {
						row = append(row, A[ri][ci])
					}
				}
				shift[len(shift)] = vals[ri]
				B = append(B, row)
			}
		}
		fmt.Println("A:", A)
		fmt.Println("B:", B)

		solve(B, append(solution, vals[r]), shift)
	}
	fmt.Println("Returning...", solution)
	return solution

}
