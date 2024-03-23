package Tree

const degree = 5

type Page struct {
	Claves   [degree]int
	Branches [degree]*Page
	Count    int
}

func NewPage() *Page {
	var claves [degree]int
	var branches [degree]*Page
	return &Page{claves, branches, 0}
}
func FullPage(actual *Page) bool {
	return actual.Count == degree-1
}
