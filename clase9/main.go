package main

import (
	"mymodule/Graph"
	"mymodule/Tree"
)

func main() {
	tree := Tree.NewTree()
	list_values := []int{6, 11, 5, 4, 8, 9, 12, 21, 14, 10, 19, 28, 3, 17, 32, 15, 16, 26, 27}

	for _, value := range list_values {
		tree.Insert(value)
		Graph.GenerateGraph_BTree(tree.Root, "BTree")

	}
	Graph.GenerateGraph_BTree(tree.Root, "BTree")
	tree.Delete(15)
	Graph.GenerateGraph_BTree(tree.Root, "Delete1")
	tree.Delete(9)
	Graph.GenerateGraph_BTree(tree.Root, "Delete2")
	tree.Delete(32)
	Graph.GenerateGraph_BTree(tree.Root, "Delete3")
}
