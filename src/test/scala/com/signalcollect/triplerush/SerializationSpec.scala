/*
 *  @author Philip Stutz
 *
 *  Copyright 2014 University of Zurich
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

import com.signalcollect.GraphBuilder
import com.signalcollect.util.TestAnnouncements
import org.scalatest.FlatSpec

class SerializationSpec extends FlatSpec with TestAnnouncements {

  "TripleRush" should "run when messages are serialized" in {
    val tr = TripleRush(graphBuilder = new GraphBuilder[Long, Any]().withMessageSerialization(true))
    try {
      tr.addEncodedTriple(1, 2, 3)
      tr.prepareExecution()
    } finally {
      tr.shutdown()
    }
  }

}
