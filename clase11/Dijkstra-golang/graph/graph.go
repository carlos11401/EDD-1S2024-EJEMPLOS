package graph

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strconv"
)

func GenerateGraph(matPesos [][]float32, infinito float32) {
	dotStructure := "graph G{\nnode [shape=circle];\n" +
		"rankdir=LR;\n"
	accum := ""
	nodes := ""
	for i := 0; i < len(matPesos[0]); i++ {
		accum += "\"" + strconv.Itoa(i) + "\"[label=\"" + strconv.Itoa(i) + "\"];\n"
		for j := 0; j < len(matPesos[0]); j++ {
			if j > i-1 {
				if matPesos[i][j] < infinito {
					nodes += "\"" + strconv.Itoa(i) + "\" -- \"" + strconv.Itoa(j) + "\" [label = \"" + fmt.Sprintf("%f", matPesos[i][j]) + "\"];\n"
				}
			}
		}
	}

	dotStructure += accum + nodes + "\n}\n"

	path := "grafo.dot"
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
	cmd, _ := exec.Command(path2, "-Tpng", "grafo.dot").Output()
	mode := int(0777)
	ioutil.WriteFile("grafo.png", cmd, os.FileMode(mode))

}

func existError(err error) bool {
	if err != nil {
		fmt.Println(err.Error())
	}
	return err != nil
}
