package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strconv"
)

const N = 6

var F [N]bool
var matPesos [N][N]float32
var infinito float32

type EstadoVertice struct {
	ultimo    int
	distancia float32
}

var D [N]EstadoVertice

func main() {
	infinito = 100000
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			matPesos[i][j] = infinito
		}
	}
	matPesos[0][1] = 4
	matPesos[0][2] = 2
	matPesos[1][0] = 4
	matPesos[1][2] = 1
	matPesos[1][3] = 5
	matPesos[2][0] = 2
	matPesos[2][1] = 1
	matPesos[2][3] = 8
	matPesos[2][4] = 10
	matPesos[3][1] = 5
	matPesos[3][2] = 8
	matPesos[3][4] = 2
	matPesos[3][5] = 6
	matPesos[4][2] = 10
	matPesos[4][3] = 2
	matPesos[4][5] = 2
	matPesos[5][3] = 6
	matPesos[5][4] = 2
	// in D is result of shorter roam
	D = caminosMinimos(D, matPesos, N)
	fmt.Println(D)
	D = Order(D)
	// get nodes that i will have to roam
	var nodesToRoam []int
	for _, estadoVertice := range D {
		nodesToRoam = append(nodesToRoam, estadoVertice.ultimo)
	}
	// sometimes some numbers is repeated so we'll delete them
	nodesToRoam = deleteRepeated(nodesToRoam)
	// add de las node that we have to roam
	nodesToRoam = append(nodesToRoam, len(matPesos[0])-1)
	fmt.Println(nodesToRoam)
	GenerateGraph(nodesToRoam)
}

func minimo(F [N]bool, D [N]EstadoVertice, n int) int {
	var mx float32
	var v int
	mx = infinito
	for j := 1; j < n; j++ {
		if !F[j] && mx >= D[j].distancia {
			mx = D[j].distancia
			v = j
		}
	}
	return v
}
func caminosMinimos(D [N]EstadoVertice, matPesos [N][N]float32, n int) [N]EstadoVertice {
	var v int
	var s = 0
	F[s] = true
	for i := 1; i < n; i++ {
		F[i] = false
		D[i].distancia = matPesos[0][i]
		D[i].ultimo = 0
	}
	for i := 1; i < n; i++ {
		v = minimo(F, D, n)
		F[v] = true
		for w := 1; w < n; w++ {
			if !F[w] {
				if D[v].distancia+matPesos[v][w] < D[w].distancia {
					D[w].distancia = D[v].distancia + matPesos[v][w]
					D[w].ultimo = v
				}
			}
		}
	}
	return D
}
func deleteRepeated(nodesToRoam []int) []int {
	var aux []int
	var result []int
	var repeated bool
	for _, node := range nodesToRoam {
		for _, num := range aux {
			if num == node {
				repeated = true
			}
		}
		if !repeated {
			result = append(result, node)
			aux = append(aux, node)
		}
		repeated = false
	}
	return result
}
func Order(ListaDesordenada [N]EstadoVertice) [N]EstadoVertice {
	var auxiliar EstadoVertice
	for i := 0; i < len(ListaDesordenada); i++ {
		for j := 0; j < len(ListaDesordenada); j++ {
			if ListaDesordenada[i].distancia < ListaDesordenada[j].distancia {
				auxiliar = ListaDesordenada[i]
				ListaDesordenada[i] = ListaDesordenada[j]
				ListaDesordenada[j] = auxiliar
			}
		}
	}
	return ListaDesordenada
}
func GenerateGraph(nodesToRoam []int) {
	const pivot = len(matPesos[0])
	var labels [2 * pivot]string
	dotStructure := "graph G{\nnode [shape=circle];\n" +
		"rankdir=LR;\n"
	accum := ""
	nodes := ""
	for i := 0; i < len(matPesos[0]); i++ {
		labels[i] = "\"" + strconv.Itoa(i) + "\"[label=\"" + strconv.Itoa(i) + "\"];\n"
		// search node to Roam
		labels[i+pivot] = "\"" + strconv.Itoa(i) + "\"[label=\"" + strconv.Itoa(i) + "\" color=lightblue2, style=filled];\n"
		// relations of nodes
		for j := 0; j < len(matPesos[0]); j++ {
			if j > i-1 {
				if matPesos[i][j] < infinito {
					nodes += "\"" + strconv.Itoa(i) + "\" -- \"" + strconv.Itoa(j) + "\" [label = \"" + fmt.Sprintf("%f", matPesos[i][j]) + "\"];\n"
				}
			}
		}
	}
	// create images and dot
	for i, nodeToRoam := range nodesToRoam {
		accum += labels[pivot] // for always show node to start
		// for first image
		if i == 0 {
			for j := 1; j < len(labels)-pivot; j++ {
				accum += labels[j]
			}
		} else if i == len(nodesToRoam)-1 {
			for j := 1; j < len(labels)-pivot; j++ {
				if j == len(labels)-(pivot+1) {
					accum += labels[len(labels)-1]
				} else {
					accum += labels[j]
				}
			}
		} else {
			for j := 1; j < len(labels)-pivot; j++ {
				if j == nodeToRoam {
					accum += labels[len(labels)-pivot+nodeToRoam]
				} else {
					accum += labels[j]
				}
			}
		}
		dotStructure += accum + nodes + "\n}\n"

		path := "grafo" + strconv.Itoa(i) + ".dot"
		//SE ESCRIBE EL ARCHIVO .DOT
		var _, err = os.Stat(path)
		if os.IsNotExist(err) {
			var file, err = os.Create(path)
			if existError(err) {
				return
			}
			defer file.Close()
			fmt.Println("Se ha creado un archivo")
		}

		var file, err2 = os.OpenFile(path, os.O_RDWR, 0644)
		if existError(err2) {
			return
		}
		defer file.Close()

		//SE ESCRIBE EN ARCHIVO
		_, err = file.WriteString(dotStructure)
		if existError(err) {
			return
		}

		// Salva los cambios
		err = file.Sync()
		if existError(err) {
			return
		}

		fmt.Println("Archivo actualizado existosamente.")

		//PARTE EN DONDE GENERO EL GRAFO
		path2, _ := exec.LookPath("dot")
		cmd, _ := exec.Command(path2, "-Tpng", "grafo"+strconv.Itoa(i)+".dot").Output()
		mode := int(0777)
		ioutil.WriteFile("grafo"+strconv.Itoa(i)+".png", cmd, os.FileMode(mode))
		dotStructure = "graph G{\nnode [shape=circle];\n" +
			"rankdir=LR;\n"
		accum = ""
	}
}
func existError(err error) bool {
	if err != nil {
		fmt.Println(err.Error())
	}
	return err != nil
}
