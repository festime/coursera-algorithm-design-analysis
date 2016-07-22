require 'set'
require 'pry'

class Graph
  attr_reader :graph, :parent_vertex_ids, :remaining_vertex_ids

  CONNECTION = 1
  DISCONNECTION = 0

  def initialize(file_name:)
    @graph = []

    array = File.readlines(file_name).map do |line|
      line.split.map(&:to_i)[1..-1]
    end.compact.unshift([])

    array.each_with_index do |adjacency_list, i|
      h = {}

      adjacency_list.each { |j| h[j] = CONNECTION }
      adjacent_vertex_list = h.keys
      @graph << h
    end.unshift({})
    @parent_vertex_ids = (0...@graph.length).to_a
    @remaining_vertex_ids = Set.new(1...@graph.length)
  end

  # def to_s
    # result = []

    # @graph.each_with_index do |hash, i|
      # adjacent_vertices = {
        # i => (hash.select { |key, value| value != nil }).keys
      # }
      # result << adjacent_vertices
    # end

    # result
  # end

  def size
    @graph.length - 1
  end

  def merge_two_vertices
    parent_vertex, child_vertex = random_edge

    # remove parent vertex self loop
    remove_edge(parent_vertex, child_vertex)
    # all vertices connected to child vertex
    # now connect to parent vertex
    for i in 1...@graph.length
      if @parent_vertex_ids[i] == child_vertex
        @parent_vertex_ids[i] = parent_vertex
      end

      if @graph[i][child_vertex]
        @graph[i][parent_vertex] = @graph[i][parent_vertex].to_i +
          @graph[i][child_vertex]
        @graph[i][child_vertex] = DISCONNECTION
        @graph[parent_vertex][i] = @graph[parent_vertex][i].to_i +
          @graph[child_vertex][i]
        @graph[child_vertex][i] = DISCONNECTION
      end
    end

    # puts "parent_vertex = #{parent_vertex}, child_vertex = #{child_vertex}"
    # puts @graph
    # puts "@parent_vertex_ids = #{@parent_vertex_ids}"
    # puts "@remaining_vertex_ids = #{@remaining_vertex_ids.to_a}"
    # puts "=" * 30
  end

  def remove_edge(parent_vertex, child_vertex)
    @graph[parent_vertex][child_vertex] = DISCONNECTION
    @graph[child_vertex][parent_vertex] = DISCONNECTION
    @remaining_vertex_ids.delete(child_vertex)
  end

  def random_edge
    edges = []

    @remaining_vertex_ids.each do |remaining_vertex_id|
      @graph[remaining_vertex_id].each do |adjacent_vertex, connection|
        edges << [remaining_vertex_id, adjacent_vertex] if connection > 0
      end
    end

    edges.sample
  end

  def unfinished?
    @remaining_vertex_ids.size > 2
  end

  def min_cut
    vertex = @remaining_vertex_ids.first
    cross_edges_number = 0

    for i in 1...@graph.length
      if @parent_vertex_ids[i] == vertex
        @graph[i].values.each { |n| cross_edges_number += n }
      end
    end

    cross_edges_number
  end
end

class GraphMinimumCut
  attr_reader :graph

  def initialize(file_name: nil)
    # @graph = Graph.new(file_name: file_name)
    @file_name = file_name
  end

  def run
    min_cut = -1
    graph_clone = Graph.new(file_name: @file_name)

    for i in 1..(graph_clone.size ** 2)
      graph_clone = Graph.new(file_name: @file_name)
      # graph_clone = @graph.clone
      # puts @graph.graph
      # puts "=" * 30

      while graph_clone.unfinished?
        graph_clone.merge_two_vertices
      end

      min_cut = graph_clone.min_cut if min_cut > graph_clone.min_cut || min_cut == -1

      # puts "graph_clone.min_cut = #{graph_clone.min_cut}"
      # puts "min_cut = #{min_cut}"
      # puts "size = #{graph_clone.size}"
    end

    min_cut
  end

  private
end
