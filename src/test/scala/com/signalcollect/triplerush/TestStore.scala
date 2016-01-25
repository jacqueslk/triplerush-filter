/*
 * Copyright (C) 2015 Cotiviti Labs (nexgen.admin@cotiviti.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.signalcollect.triplerush

import java.net.ServerSocket
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.reflect.runtime.universe
import scala.util.{ Failure, Success, Try }
import org.scalatest.fixture.NoArg
import com.signalcollect.GraphBuilder
import com.signalcollect.configuration.Akka
import com.typesafe.config.{ Config, ConfigFactory }
import org.scalatest.concurrent.ScalaFutures
import akka.actor.ActorSystem
import scala.concurrent.duration.DurationInt
import akka.actor.Props
import com.signalcollect.triplerush.mapper.DistributedTripleMapperFactory
import com.signalcollect.nodeprovisioning.cluster.ClusterNodeProvisionerActor
import akka.actor.ActorRef
import akka.util.Timeout
import com.signalcollect.nodeprovisioning.cluster.RetrieveNodeActors
import org.scalatest.concurrent.ScalaFutures._
import akka.pattern.ask
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object TestStore {

  val numberOfCoresInTests = 2

  def instantiateUniqueStore(): TripleRush = {
    val graphBuilder = instantiateUniqueGraphBuilder()
    TripleRush(graphBuilder = graphBuilder)
  }

  def instantiateDistributedStore(masterPort: Int, numberOfNodes: Int)(): (TripleRush, Seq[ActorSystem]) = {
    val masterPort = TestStore.freePort
    val masterSystem = TestStore.instantiateUniqueActorSystem(port = masterPort, seedPortOption = Some(masterPort), actorSystemName = "ClusterSystem")
    val slaveSystems = for (slaveId <- 1 to numberOfNodes) yield instantiateUniqueActorSystem(seedPortOption = Some(masterPort), actorSystemName = "ClusterSystem")
    implicit val timeout = Timeout(30.seconds)
    val mapperFactory = DistributedTripleMapperFactory
    val masterActor = masterSystem.actorOf(Props(classOf[ClusterNodeProvisionerActor], 1000,
      "", numberOfNodes), "ClusterMasterBootstrap")
    val nodeActorsFuture = (masterActor ? RetrieveNodeActors).mapTo[Array[ActorRef]]
    val nodeActors = Await.result(nodeActorsFuture, 30.seconds)
    assert(nodeActors.length == numberOfNodes)
    val graphBuilder = new GraphBuilder[Long, Any]().
      withActorSystem(masterSystem).
      withPreallocatedNodes(nodeActors)
    val tr = TripleRush(
      graphBuilder = graphBuilder,
      tripleMapperFactory = Some(mapperFactory))
    (tr, slaveSystems)
  }

  def customClusterConfig(actorSystemName: String, port: Int, seedPort: Int, seedIp: String = "127.0.0.1"): Config = {
    ConfigFactory.parseString(
      s"""|akka.clustering.name=$actorSystemName
          |akka.clustering.seed-ip=$seedIp
          |akka.clustering.seed-port=$seedPort
          |akka.remote.netty.tcp.port=$port
          |akka.cluster.seed-nodes=["akka.tcp://"${actorSystemName}"@"${seedIp}":"${seedPort}]"""
        .stripMargin)
  }

  def instantiateUniqueActorSystem(cores: Int = numberOfCoresInTests, port: Int = freePort, seedPortOption: Option[Int] = None, actorSystemName: String = "TripleRushTestSystem"): ActorSystem = {
    val defaultAkkaConfig = Akka.config(
      serializeMessages = None,
      loggingLevel = None,
      kryoRegistrations = Kryo.defaultRegistrations,
      kryoInitializer = Some("com.signalcollect.triplerush.serialization.TripleRushKryoInit"),
      numberOfCores = cores)
    val seedPort = seedPortOption.getOrElse(port)
    val customAkkaConfig = customClusterConfig(actorSystemName = actorSystemName, port = port, seedPort = seedPort)
      .withFallback(defaultAkkaConfig)
    ActorSystem(actorSystemName, customAkkaConfig)
  }

  def instantiateUniqueGraphBuilder(cores: Int = numberOfCoresInTests): GraphBuilder[Long, Any] = {
    new GraphBuilder[Long, Any]()
      .withActorSystem(instantiateUniqueActorSystem(cores))
  }

  private[this] val portUsageTracker = new AtomicInteger(2500)

  def freePort: Int = {
    @tailrec def attemptToBindAPort(failuresSoFar: Int): Int = {
      val checkedPort = portUsageTracker.incrementAndGet
      val socketTry = Try(new ServerSocket(checkedPort))
      socketTry match {
        case Success(s) =>
          s.close()
          checkedPort
        case Failure(f) =>
          if (failuresSoFar > 10) {
            throw f
          } else {
            attemptToBindAPort(failuresSoFar + 1)
          }
      }
    }
    attemptToBindAPort(0)
  }

}

class TestStore(storeInitializer: => TripleRush) extends NoArg {

  lazy val tr: TripleRush = storeInitializer
  lazy implicit val model = tr.getModel
  lazy implicit val system = tr.graph.system

  def this() = this(TestStore.instantiateUniqueStore)

  def close(): Unit = {
    model.close()
    tr.close()
    Await.ready(tr.graph.system.terminate(), Duration.Inf)
  }

  override def apply(): Unit = {
    try {
      super.apply()
    } finally {
      close()
    }
  }

}

class DistributedTestStore(storeInitializer: => (TripleRush, Seq[ActorSystem])) extends NoArg {

  lazy val (tr, slaves): (TripleRush, Seq[ActorSystem]) = storeInitializer
  lazy implicit val model = tr.getModel
  lazy implicit val system = tr.graph.system

  def this() = {
    this(TestStore.instantiateDistributedStore(TestStore.freePort, 5))
  }

  def close(): Unit = {
    model.close()
    tr.close()
    Await.ready(tr.graph.system.terminate(), Duration.Inf)
    Await.ready(Future.sequence(slaves.map(_.terminate)), 30.seconds)
  }

  override def apply(): Unit = {
    try {
      super.apply()
    } finally {
      close()
    }
  }

}
