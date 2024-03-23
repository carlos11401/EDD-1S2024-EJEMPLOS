package Tree

import (
	"fmt"
)

type Tree struct {
	Root *Page
}

func NewTree() *Tree {
	return &Tree{nil}
}

// Insert to insert
func (tree *Tree) Insert(value int) {
	insert(&tree.Root, value)
}
func insert(root **Page, value int) {
	go_up := false
	median := 0
	var newPage *Page
	Push(*root, value, &go_up, &median, &newPage)
	if go_up { //si se produjo una reorganizacion de nodos lo cual ser dividio la raiz entonces, la bandera sube_arriba lo indica
		p := NewPage()
		p.Count = 1
		p.Claves[1] = median
		p.Branches[0] = *root
		p.Branches[1] = newPage
		*root = p
	}
}
func Push(actualPage *Page, value int, go_up *bool, median *int, newPage **Page) {
	k := 0
	if actualPage == nil {
		*go_up = true
		*median = value
		newPage = nil
	} else {
		// search the node where the value should be inserted
		// k is the position of the branch
		node_repeated := Search_node_on_page(actualPage, value, &k)
		if node_repeated {
			fmt.Println("Clave Duplicada: ", value)
			*go_up = false
			return
		}
		Push(actualPage.Branches[k], value, go_up, median, newPage)
		/* devuelve control vuelve por el camino de busqueda*/
		if *go_up {
			if FullPage(actualPage) {
				DividePage(actualPage, *median, *newPage, k, median, newPage)
			} else {
				*go_up = false
				PushNode(actualPage, *median, *newPage, k)
			}
		}
	}
}
func DividePage(actualPage *Page, value int, rd *Page, k int, median *int, newPage **Page) {
	// newPage is the new node (right) that will be created
	// actualPage is the original node (left)
	posMedian := 3
	*newPage = NewPage()
	for i := posMedian + 1; i < degree; i++ {
		// move the keys and branches to the new node (right)
		(*newPage).Claves[i-posMedian] = actualPage.Claves[i]
		(*newPage).Branches[i-posMedian] = actualPage.Branches[i]
	}
	(*newPage).Count = (degree - 1) - posMedian // keys in the new node
	actualPage.Count = posMedian                // keys in the original node

	// Insert the key and the branch in the corresponding node
	if k <= degree/2 {
		PushNode(actualPage, value, rd, k) // insert the new value in the original node(left)
	} else {
		PushNode(*newPage, value, rd, k-posMedian) // insert the new value in the new node(right)
	}

	// save the median key
	*median = actualPage.Claves[actualPage.Count]

	// the branch of the new node (right) is the last branch of the original node (left)
	(*newPage).Branches[0] = actualPage.Branches[actualPage.Count]

	actualPage.Count--
}
func PushNode(actual *Page, value int, rd *Page, k int) {
	// move the keys and branches to the right
	for i := actual.Count; i >= k+1; i-- {
		actual.Claves[i+1] = actual.Claves[i]
		actual.Branches[i+1] = actual.Branches[i]
	}
	actual.Claves[k+1] = value
	actual.Branches[k+1] = rd
	actual.Count++
}
func Search_node_on_page(actual *Page, value int, k *int) bool {
	// k is the position of the branch
	var repeated bool
	// if the value is less than the first key
	if value < actual.Claves[1] {
		*k = 0 // the branch is the first one
		repeated = false
	} else // if the value is greater than the last key
	{
		*k = actual.Count
		for (value < actual.Claves[*k]) && (*k > 1) { // while the value is less than the key and the key is not the first one
			*k--
		}
		repeated = value == actual.Claves[*k] // if the value is equal to the key
	}
	return repeated
}

// Delete to delete
func (this *Tree) Delete(valor int) {
	delete(&this.Root, valor)
}
func delete(root **Page, value int) {
	found := false
	deleteRegister(*root, value, &found)
	if found {
		if (*root).Count == 0 {
			/* la raiz esta vacia, se libera el nodo*/
			p := NewPage()
			*p = **root
			*root = (*root).Branches[0]
		}
	} // else la clave no esta en le arbol
}
func deleteRegister(actual *Page, value int, found *bool) {
	var k int
	if actual != nil {
		*found = Search_node_on_page(actual, value, &k)
		if *found {
			if actual.Branches[k-1] == nil {
				quitar(actual, k)
			} else {
				sucesor(actual, k)
				deleteRegister(actual.Branches[k], actual.Claves[k], found)
			}
		} else {
			deleteRegister(actual.Branches[k], value, found)
		}
		if actual.Branches[k] != nil {
			if actual.Branches[k].Count < degree/2 {
				restablecer(actual, k)
			}
		}
	} else {
		*found = false
	}
}
func restablecer(actual *Page, k int) {
	if k > 0 {
		if actual.Branches[k-1].Count > degree/2 {
			moveRight(actual, k)
		} else {
			combine(actual, k)
		}
	} else {
		if actual.Branches[1].Count > degree/2 {
			moveLeft(actual, 1)
		} else {
			combine(actual, 1)
		}
	}
}
func combine(actual *Page, k int) {
	leftNode := NewPage()
	q := NewPage()

	q = actual.Branches[k]
	leftNode = actual.Branches[k-1]

	leftNode.Count++
	leftNode.Claves[leftNode.Count] = actual.Claves[k]
	leftNode.Branches[leftNode.Count] = q.Branches[0]

	for j := 1; j <= q.Count; j++ {
		leftNode.Count++
		leftNode.Claves[leftNode.Count] = q.Claves[j]
		leftNode.Branches[leftNode.Count] = q.Branches[j]
	}
	for j := k; j <= actual.Count-1; j++ {
		actual.Claves[j] = actual.Claves[j+1]
		actual.Branches[j] = actual.Branches[j+1]
	}
	actual.Count--
}
func moveLeft(actual *Page, k int) {
	problemNode := NewPage()
	rightNode := NewPage()
	problemNode = actual.Branches[k-1]
	rightNode = actual.Branches[k]

	problemNode.Count++
	problemNode.Claves[problemNode.Count] = actual.Claves[k]
	problemNode.Branches[problemNode.Count] = rightNode.Branches[0]

	actual.Claves[k] = rightNode.Claves[1]
	rightNode.Branches[1] = rightNode.Branches[0]
	rightNode.Count--

	for i := 1; i <= rightNode.Count; i++ {
		rightNode.Claves[i] = rightNode.Claves[i+1]
		rightNode.Branches[i] = rightNode.Branches[i+1]
	}
}
func moveRight(actual *Page, k int) {
	problemNode := NewPage()
	leftNode := NewPage()
	problemNode = actual.Branches[k]
	leftNode = actual.Branches[k-1]
	for j := problemNode.Count; j >= 1; j-- {
		problemNode.Claves[j+1] = problemNode.Claves[j]
		problemNode.Branches[j+1] = problemNode.Branches[j]
	}
	problemNode.Count++
	problemNode.Branches[1] = problemNode.Branches[0]

	problemNode.Claves[1] = actual.Claves[k]

	actual.Claves[k] = leftNode.Claves[leftNode.Count]
	problemNode.Branches[0] = leftNode.Branches[leftNode.Count]
	leftNode.Count--
}
func sucesor(actual *Page, k int) {
	q := NewPage()
	q = actual.Branches[k]
	for q.Branches[0] != nil {
		q = q.Branches[0]
	}
	actual.Claves[k] = q.Claves[1]
}
func quitar(actual *Page, k int) {
	for j := k + 1; j <= actual.Count; j++ {
		actual.Claves[j-1] = actual.Claves[j]
		actual.Branches[j-1] = actual.Branches[j]
	}
	actual.Count--
}
