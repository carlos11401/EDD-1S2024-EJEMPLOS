package Graph

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strconv"

	"mymodule/Tree"
)

func GenerateGraph_BTree(root *Tree.Page, name string) {
	acum := "digraph G\n" +
		"{\n node	[shape = record,height=.1];\n"

	if root != nil {
		acumEnlace := ""
		contNodo := 0
		contAux := 0

		var queue []*Tree.Page
		queue = Enqueue(queue, root)

		for len(queue) != 0 {
			tmp := queue[0]
			queue = Dequeue(queue)
			printCode(*tmp, &acum, &contNodo, &contAux, &acumEnlace)
			for i := 0; i <= tmp.Count; i++ {
				if tmp.Branches[i] != nil {
					queue = Enqueue(queue, tmp.Branches[i])
				}
			}
			contNodo++
		}
		acum += "\n" + acumEnlace
	}
	acum += "}\n"

	path := "Graph/" + name + ".dot"
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
	_, err = file.WriteString(acum)
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
	cmd, _ := exec.Command(path2, "-Tpng", "Graph/"+name+".dot").Output()
	mode := int(0777)
	ioutil.WriteFile("Graph/"+name+".png", cmd, os.FileMode(mode))
}
func printCode(actual Tree.Page, acum *string, contNodo *int, contAux *int, enlace *string) {
	*acum += "node" + strconv.Itoa(*contNodo) + "[label=\""
	*acum += "<r0>"
	if actual.Branches[0] != nil {
		*enlace += "\"node" + strconv.Itoa(*contNodo) + "\":r0 ->"
		*contAux += 1
		*enlace += "\"node" + strconv.Itoa(*contAux) + "\"\n"
	}
	for i := 1; i <= actual.Count; i++ {
		*acum += "|"
		*acum += "<c" + strconv.Itoa(i) + "> " + strconv.Itoa(actual.Claves[i])
		*acum += "|<r" + strconv.Itoa(i) + ">"
		if actual.Branches[i] != nil {
			*enlace += "\"node" + strconv.Itoa(*contNodo) + "\":r" + strconv.Itoa(i) + " -> "
			*contAux += 1
			*enlace += "\"node" + strconv.Itoa(*contAux) + "\"\n"
		}
	}
	*acum += "\"];\n"
}

func existError(err error) bool {
	if err != nil {
		fmt.Println(err.Error())
	}
	return err != nil
}

func Enqueue(queue []*Tree.Page, element *Tree.Page) []*Tree.Page {
	queue = append(queue, element) // Simply append to Enqueue.
	fmt.Println("Enqueued:", element)
	return queue
}
func Dequeue(queue []*Tree.Page) []*Tree.Page {
	return queue[1:] // Slice off the element once it is dequeued.
}
