/*
 *  @author Philip Stutz
 *  @author Mihaela Verman
 *  
 *  Copyright 2013 University of Zurich
 *      
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *         http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */

package com.signalcollect.triplerush

object Mapping {
  private var id2String = Map[Int, String]((0 -> "*"))
  private var string2Id = Map[String, Int](("*" -> 0))
  private var maxId = 0
  private var minId = 0
  private var abbreviations = Map[String, String]() //original = key, abbreviation = value

  private def abbreviate(s: String): String = {
    val abbreviatedString = abbreviations.keys.foldLeft(s) {
      case (intermediateString, expandedSequence) =>
        intermediateString.replace(expandedSequence, abbreviations(expandedSequence))
    }
    abbreviatedString
  }

  def register(s: String, isVariable: Boolean = false): Int = {
    val abbreviation = abbreviate(s)
    synchronized {
      if (!string2Id.contains(abbreviation)) {
        val id = {
          if (isVariable) {
            minId -= 1
            minId
          } else {
            maxId += 1
            maxId
          }
        }
        string2Id += ((abbreviation, id))
        id2String += ((id, abbreviation))
        id
      } else {
        string2Id(abbreviation)
      }
    }
  }
  def getId(s: String): Int = {
    string2Id(abbreviate(s))
  }
  def getString(id: Int): String = {
    id2String(id)
  }
  def existsMappingForString(s: String): Boolean = {
    string2Id.contains(abbreviate(s))
  }

  def setAbbreviations(a: Map[String, String]) = synchronized {
    abbreviations = a
  }
}