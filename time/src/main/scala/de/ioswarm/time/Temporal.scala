package de.ioswarm.time

trait Temporal {

  def epoch: Long
  def offset: Offset

  def time: Long = epoch + offset.millis

}