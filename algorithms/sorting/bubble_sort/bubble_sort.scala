// Start writing your ScalaFiddle code here
def bubble_sort[T <% Ordered[T]](src: List[T]): List[T] = {
    def my_bs[T <% Ordered[T]](src: List[T], res: List[T], is_swapped: Boolean): List[T] = {
      src match {
        case Nil => res
        case s::Nil => is_swapped match {
          case true => my_bs(res:+s, List.empty, false)
          case false => res:+s
        }
        case x::xs => x.compareTo(xs.head) match {
          case 1 => my_bs(x::xs.tail, res:+xs.head, true)
          case _ => my_bs(xs, res:+x, is_swapped) 
        }
      }
    }    
    my_bs(src, List.empty, false)
	
  }

val test = List(7, 2, 5, 10, 4, 9, 12)
println(bubble_sort(test))
  