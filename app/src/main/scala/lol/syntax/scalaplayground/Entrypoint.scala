package lol.syntax.scalaplayground

import lol.syntax.scalaplayground.Or.Both

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

extension [A](rcv: A) inline def |>[B](f: A => B): B = f(rcv)

def sum(nums: List[Int]): Int =
    nums match {
        case Nil          => 0
        case head :: tail => head + sum(tail)
    }

def min(nums: List[Int]): Int =
    nums match {
        case Nil          => Int.MaxValue
        case head :: tail => math.min(head, min(tail))
    }

def optionMin(nums: List[Int]): Option[Int] =
    nums match {
        case Nil          => None
        case head :: tail => optionMin(tail).map(math.min(head, _)).orElse(Some(head))
    }

@tailrec
def find[A](data: List[A])(predicate: A => Boolean): Option[A] =
    data match {
        case Nil          => None
        case head :: tail => if predicate(head) then Some(head) else find(tail)(predicate)
    }

enum Or[+A, +B]:
    case Left(a: A)
    case Right(b: B)
    case Both(a: A, b: B)

def merge[A, B, C](a: List[A], b: List[B])(f: Or[A, B] => C): List[C] =
    (a, b) match {
        case (h1 :: t1, h2 :: t2) => f(Or.Both(h1, h2)) :: merge(t1, t2)(f)
        case (head :: tail, Nil)  => f(Or.Left(head)) :: merge(tail, List.empty[B])(f)
        case (Nil, head :: tail)  => f(Or.Right(head)) :: merge(List.empty[A], tail)(f)
        case (Nil, Nil)           => List.empty[C]
    }

def product(data: List[Int]): Int =
    @tailrec
    def logic(remaining: List[Int], acc: Int): Int =
        remaining match {
            case head :: tail => logic(tail, acc * head)
            case Nil          => acc
        }
    logic(remaining = data, acc = 1)

def max(data: List[Int]): Option[Int] =
    @tailrec
    def logic(remaining: List[Int], current: Int): Int =
        remaining match {
            case head :: tail => logic(tail, math.max(current, head))
            case Nil          => current
        }
    data match {
        case head :: tail => Some(logic(remaining = tail, current = head))
        case Nil          => None
    }

@tailrec
def forall[A](data: List[A])(f: A => Boolean): Boolean =
    data match {
        case Nil          => true
        case head :: tail => f(head) && forall(tail)(f)
    }

def reverse[A](data: List[A]): List[A] =
    @tailrec
    def _reverse(input: List[A], buffer: List[A]): List[A] =
        input match {
            case Nil          => buffer
            case head :: tail => _reverse(tail, head :: buffer)
        }
    _reverse(input = data, buffer = List.empty[A])

def map[A, B](data: List[A])(f: A => B): List[B] =
    @tailrec
    def _map(input: List[A], buffer: List[B]): List[B] =
        input match {
            case Nil          => reverse(buffer)
            case head :: tail => _map(tail, f(head) :: buffer)
        }
    _map(input = data, buffer = List.empty[B])

def foldLeft[A, B](data: List[A], init: B)(f: (B, A) => B): B =
    @tailrec
    def _fold(input: List[A], acc: B): B =
        input match {
            case Nil          => acc
            case head :: tail => _fold(tail, f(acc, head))
        }
    _fold(input = data, acc = init)

@main
def entrypoint(): Unit =
    // reimplementation of some of above function using `foldLeft`
    def sum(data: List[Int]): Int     = foldLeft(data, 0)(_ + _)
    def product(data: List[Int]): Int = foldLeft(data, 1)(_ * _)
    def min(data: List[Int]): Option[Int] = foldLeft(data, Option.empty[Int]) { (acc, current) =>
        acc map (math.min(_, current)) orElse Some(current)
    }
    def map[A, B](data: List[A])(f: A => B): List[B] = foldLeft(data, List.empty[B]) {
        (acc, current) => f(current) :: acc
    }
    def reverse[A](data: List[A]): List[A] = foldLeft(data, List.empty[A]) { (acc, current) =>
        current :: acc
    }
    def forall[A](data: List[A])(f: A => Boolean): Boolean = foldLeft(data, true)(_ && f(_))
