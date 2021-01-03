
// https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

    // Complete the activityNotifications function below.
    def activityNotifications(expenditure: Array[Int], d: Int): Int = {
        def getMedian(a: Array[Int]): Int = {
            a.sortWith(_ < _).drop(a.length/2).head
        }
        
        def comp_notifs(e: Array[Int], p: Int, res: Int, d: Int): Int = {
            p match {
                case p if p == e.length => res
                case p if p < d => comp_notifs(e, p+1, res, d)
                case p => {
                
                    val median = getMedian(e.slice(p-d, p))
                    e(p) match {
                        case v if v >= 2*median => comp_notifs(e, p+1, res+1, d)
                        case _ => comp_notifs(e, p+1, res, d)
                    }
                }
            }
        }
        comp_notifs(expenditure, 0, 0, d)
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val nd = stdin.readLine.split(" ")

        val n = nd(0).trim.toInt

        val d = nd(1).trim.toInt

        val expenditure = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = activityNotifications(expenditure, d)

        printWriter.println(result)

        printWriter.close()
    }
}


