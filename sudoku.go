package main

import (
	"flag"
	"fmt"
	"strconv"
	"strings"
)

type Sudoku [9][9]int

var Nums = [9]int{1, 2, 3, 4, 5, 6, 7, 8, 9}

var solution = 0

var example = Sudoku{
	{5, 3, 0,   0, 7, 0,   0, 0, 0},
	{6, 0, 0,   1, 9, 5,   0, 0, 0},
	{0, 9, 8,   0, 0, 0,   0, 6, 0},

	{8, 0, 0,   0, 6, 0,   0, 0, 3},
	{4, 0, 0,   8, 0, 3,   0, 0, 1},
	{7, 0, 0,   0, 2, 0,   0, 0, 6},

	{0, 6, 0,   0, 0, 0,   2, 8, 0},
	{0, 0, 0,   4, 1, 9,   0, 0, 5},
	{0, 0, 0,   0, 8, 0,   0, 7, 9},
}

var (
	v = flag.Bool("v", false, "Print all possibilities for unknown squares")
	l = flag.Args()
)

func main() {
	flag.Parse()
	var puzzles []*Sudoku
	for _, p := range l {
		puzzles = append(puzzles, StrToSud(p))
	}
	if len(l) == 0 {
		if *v {
			example.ShowPossible()
		}
		fmt.Printf("SudokuId: %v\n", SudToStr(&example))
		example.Solve()
		solution = 0
	}
	for _, p := range puzzles {
		if *v {
			p.ShowPossible()
		}
		fmt.Printf("SudokuIdStr: %v\n", SudToStr(p))
		p.Solve()
		solution = 0
	}
}

// Possible checks whether n is a valid number at <x,y> in a Sudoku.
func (s *Sudoku) Possible(x int, y int, n int) bool {
	for i := 0; i <= 8; i++ {
		if s[y][i] == n {
			return false
		}

		if s[i][x] == n {
			return false
		}
	}

	sqX := x / 3
	sqY := y / 3
	for j := 3*sqY; j <= 3*sqY+2; j++ {
		for i := 3*sqX; i <= 3*sqX+2; i++ {
			if s[j][i] == n {
				return false
			}
		}
	}

	return true
}

// ShowPossible lists all valid solutions for each unknown square in a Sudoku.
func (s *Sudoku) ShowPossible() {
	for j := 0; j <= 8; j++ {
		for i := 0; i <= 8; i++ {
			if s[j][i] == 0 {
				fmt.Printf("s[%v][%v] = 0. Possibles: ", j, i)
				for _, n := range Nums {
					if s.Possible(i, j, n) {
						fmt.Print(n, " ")
					}
				}
				fmt.Println()
			}
		}
	}
}

// Solve the Sudoku. Solve will try every possible solution in unknown squares,
// and will print all complete and valid solutions.
func (s *Sudoku) Solve() {
	for j := 0; j <= 8; j++ {
		for i := 0; i <= 8; i++ {
			if s[j][i] == 0 {
				for _, n := range Nums {
					if s.Possible(i, j, n) {
						s[j][i] = n
						s.Solve()
						s[j][i] = 0
					}
				}
				return
			}
		}
	}
	solution++
	s.Print()
}

// Print a Sudoku, with the the solution number if there are multiple possible
// solutions to the Sudoku.
func (s *Sudoku) Print() {
	n := len(strconv.Itoa(solution))
	lpad := strings.Repeat(" ", (7-n)/2 )
	rpad := strings.Repeat(" ", (7-n)/2 )
	width := len(lpad)+ n +len(rpad)
	fill := ""
	if width >= 7 {
		fill = ""
	} else {
		fill= " "
	}
	fmt.Println("\t" + strings.Repeat(" ", 21))
	fmt.Println("\t" + strings.Repeat("#", 7) + lpad + strconv.Itoa(solution) + rpad + fill + strings.Repeat("#", 7))
	fmt.Println("\t" + strings.Repeat(" ", 21))
	for j := range s {
		fmt.Print("\t")
		for i := range s[j] {
			fmt.Print(s[j][i], " ")
			if (i+1) % 3 == 0 && i != 8{
				fmt.Print("¦ ")
			}
		}
		fmt.Println()
		if (j+1) % 3 == 0  && j != 8 {
			fmt.Println("\t- - - - - - - - - - -")
		}
	}
}

// StrToSud converts a string of sudoku square values to a nested array.
// Example input: 534678912672195348198342567859761423426853791713924856961537284287419635345286179
func StrToSud(str string) *Sudoku {
	var sud = Sudoku{}
	runes := []rune(str)
	for n, r := range runes {
		x, err := strconv.Atoi(string(r))
		if err != nil {
			return &sud
		}
		sud[n/9][n%9] = x
	}
	return &sud
}

// SudToStr concatenates a nested array to a string of values.
// Example input: Sudoku{
//	{5, 3, 0,   0, 7, 0,   0, 0, 0},
//	{6, 0, 0,   1, 9, 5,   0, 0, 0},
//	{0, 9, 8,   0, 0, 0,   0, 6, 0},
//
//	{8, 0, 0,   0, 6, 0,   0, 0, 3},
//	{4, 0, 0,   8, 0, 3,   0, 0, 1},
//	{7, 0, 0,   0, 2, 0,   0, 0, 6},
//
//	{0, 6, 0,   0, 0, 0,   2, 8, 0},
//	{0, 0, 0,   4, 1, 9,   0, 0, 5},
//	{0, 0, 0,   0, 8, 0,   0, 7, 9},
//}
func SudToStr(sud *Sudoku) (str string) {
	for j := 0; j <= 8; j++ {
		for i := 0; i <= 8; i++ {
			str += strconv.Itoa(sud[j][i])
		}
	}
	return
}