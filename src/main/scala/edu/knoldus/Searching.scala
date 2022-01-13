package edu.knoldus

import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean ={
    @tailrec
    def binaryhelper(array: Array[Int],num: Int,start: Int,end: Int): Boolean= {
      if(start>end) return false
      val mid=start + (end-start)/2
      if(array(mid)==num) {
        true
      }
      else if(array(mid)>num) {
        binaryhelper(array,num,start,mid-1)
      } else {
        binaryhelper(array,num,mid + 1,end)
      }

    }
    binaryhelper(array,elem,0,array.length-1)

    }


  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if(array.headOption ==None){
      false
    }
    else{
      if(array.head==elem) {
        true
      }
      else {
        linearSearch(array.tail,elem)
      }
    }

  }

}
