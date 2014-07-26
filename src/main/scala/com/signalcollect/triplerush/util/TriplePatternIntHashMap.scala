/*
 *  @author Philip Stutz
 *  @author Christian Tschanz
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
 */

package com.signalcollect.triplerush.util

import com.signalcollect.triplerush.EfficientIndexPattern._
import scala.util.hashing.MurmurHash3._

/*
 * HashMap Long keys and Int values.
 */
class TriplePatternIntHashMap(
  initialSize: Int = 32768,
  rehashFraction: Float = 0.75f) {
  assert(initialSize > 0)
  final var maxSize = nextPowerOfTwo(initialSize)
  assert(1.0f >= rehashFraction && rehashFraction > 0.1f, "Unreasonable rehash fraction.")
  assert(maxSize > 0 && maxSize >= initialSize, "Initial size is too large.")
  private[this] final var maxElements: Int = (rehashFraction * maxSize).floor.toInt
  private[this] final var values = new Array[Int](maxSize)
  private[this] final var keys = new Array[Long](maxSize) // 0 means empty
  private[this] final var mask = maxSize - 1

  final def size: Int = numberOfElements
  final def isEmpty: Boolean = numberOfElements == 0
  private[this] final var numberOfElements = 0

  final def clear {
    keys = new Array[Long](maxSize)
    numberOfElements = 0
  }

  def toScalaMap: Map[Long, Int] = {
    keys.zip(values).filter(_._1 != 0).toMap
  }

  private[this] final def tryDouble {
    // 1073741824 is the largest size and cannot be doubled anymore.
    if (maxSize != 1073741824) {
      val oldValues = values
      val oldKeys = keys
      val oldNumberOfElements = numberOfElements
      maxSize *= 2
      maxElements = (rehashFraction * maxSize).floor.toInt
      values = new Array[Int](maxSize)
      keys = new Array[Long](maxSize)
      mask = maxSize - 1
      numberOfElements = 0
      var i = 0
      var elementsMoved = 0
      while (elementsMoved < oldNumberOfElements) {
        val oldKey = oldKeys(i)
        if (oldKey != 0) {
          put(oldKey, oldValues(i))
          elementsMoved += 1
        }
        i += 1
      }
    }
  }

  @inline final def foreach(f: (Long, Int) => Unit) {
    var i = 0
    var elementsProcessed = 0
    while (elementsProcessed < numberOfElements) {
      val key = keys(i)
      if (key != 0) {
        val value = values(i)
        f(key, value)
        elementsProcessed += 1
      }
      i += 1
    }
  }

  /**
   * Like foreach, but removes the entry after applying the function.
   */
  @inline final def process(f: (Long, Int) => Unit) {
    var i = 0
    var elementsProcessed = 0
    while (elementsProcessed < numberOfElements) {
      val key = keys(i)
      if (key != 0) {
        val value = values(i)
        f(key, value)
        elementsProcessed += 1
        keys(i) = 0l
      }
      i += 1
    }
    numberOfElements = 0
  }
  
  final def remove(key: Long) {
    remove(key, true)
  }

  private final def remove(key: Long, optimize: Boolean) {
    var position = keyToPosition(key)
    var keyAtPosition = keys(position)
    while (keyAtPosition != 0 && (key != keyAtPosition)) {
      position = (position + 1) & mask
      keyAtPosition = keys(position)
    }
    // We can only remove the entry if it was found.
    if (keyAtPosition != 0) {
      keys(position) = 0
      numberOfElements -= 1
      if (optimize) {
        optimizeFromPosition((position + 1) & mask)
      }
    }
  }

  // Try to reinsert all elements that are not optimally placed until an empty position is found.
  // See http://stackoverflow.com/questions/279539/best-way-to-remove-an-entry-from-a-hash-table
  private[this] final def optimizeFromPosition(startingPosition: Int) {
    var currentPosition = startingPosition
    var keyAtPosition = keys(currentPosition)
    while (isCurrentPositionOccupied) {
      val perfectPositionForEntry = keyToPosition(keyAtPosition)
      if (perfectPositionForEntry != currentPosition) {
        // We try to optimize the placement of the entry by removing and then reinserting it.
        val value = values(currentPosition)
        removeCurrentEntry
        put(keyAtPosition, value)
      }
      advance
    }
    @inline def advance {
      currentPosition = ((currentPosition + 1) & mask)
      keyAtPosition = keys(currentPosition)
    }
    @inline def isCurrentPositionOccupied = {
      keyAtPosition != 0
    }
    @inline def removeCurrentEntry {
      keys(currentPosition) = 0
      numberOfElements -= 1
    }
  }

  final def apply(key: Long) = get(key)

  @inline final def get(key: Long): Int = {
    var position = keyToPosition(key)
    var keyAtPosition = keys(position)
    while (keyAtPosition != 0 && key != keyAtPosition) {
      position = (position + 1) & mask
      keyAtPosition = keys(position)
    }
    if (keyAtPosition != 0) {
      values(position)
    } else {
      0
    }
  }

  final def update(key: Long, value: Int) {
    put(key, value)
  }

  /**
   * Key 0 is not allowed!
   * Returns if an existing entry was overridden.
   */
  def put(key: Long, value: Int): Boolean = {
    assert(key != 0, "Key cannot be 0")
    var position = keyToPosition(key)
    var keyAtPosition = keys(position)
    while (keyAtPosition != 0 && key != keyAtPosition) {
      position = (position + 1) & mask
      keyAtPosition = keys(position)
    }
    val overridden = keyAtPosition == key
    if (!overridden) {
      keys(position) = key
      numberOfElements += 1
      if (numberOfElements >= maxElements) {
        tryDouble
        if (numberOfElements >= maxSize) {
          throw new OutOfMemoryError("The hash map is full and cannot be expanded any further.")
        }
      }
      put(key, value)
    } else {
      values(position) = value
    }
    overridden
  }

  private[this] final def keyToPosition(efficientIndexPattern: Long): Int = {
    val triplePatternHash = finalizeHash(mixLast(efficientIndexPattern.extractFirst, efficientIndexPattern.extractSecond), 3)
    triplePatternHash & mask
  }

  private[this] final def nextPowerOfTwo(x: Int): Int = {
    var r = x - 1
    r |= r >> 1
    r |= r >> 2
    r |= r >> 4
    r |= r >> 8
    r |= r >> 16
    r + 1
  }

}
