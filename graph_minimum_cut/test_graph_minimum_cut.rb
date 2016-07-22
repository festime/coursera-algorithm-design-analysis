require 'minitest/autorun'
require 'minitest/pride'
require 'pp'
require_relative './graph_minimum_cut'

class TestGraphMinimumCut < Minitest::Test
  # def test_read_file_function
    # assert_equal(
      # GraphMinimumCut.read_input_from_file(
        # './graph_minimum_cut/small_input.txt'
      # ),
      # [[],
       # [2, 3, 4, 7],
       # [1, 3, 4],
       # [1, 2, 4],
       # [1, 2, 3, 5],
       # [4, 6, 7, 8],
       # [5, 7, 8],
       # [1, 5, 6, 8],
       # [5, 6, 7]]
    # )
  # end

  def test_on_small_input
    # graph = Graph.new(
      # file_name: './graph_minimum_cut/small_input.txt'
    # )
    # pp graph.graph
    # pp graph.random_edge
    # pp graph.graph
    # pp graph.size
    # pp graph.remaining_vertex_ids
    # pp graph.parent_vertex_ids

    assert_equal(
      2,
      GraphMinimumCut.new(file_name: './graph_minimum_cut/small_input.txt').run
    )
  end

  def test_on_permutated_small_input
    assert_equal(
      2,
      GraphMinimumCut.new(file_name: './graph_minimum_cut/small_input_after_permutations.txt').run
    )
  end

  def test_on_another_small_input
    assert_equal(
      1,
      GraphMinimumCut.new(file_name: './graph_minimum_cut/another_small_input.txt').run
    )
  end

  def test_on_permutated_another_small_input
    assert_equal(
      1,
      GraphMinimumCut.new(file_name: './graph_minimum_cut/permutated_another_small_input.txt').run
    )
  end

  def test_on_large_input
    assert_equal(
      3,
      GraphMinimumCut.new(file_name: './graph_minimum_cut/large_input.txt').run
    )
  end

  def test_on_test_input
    puts GraphMinimumCut.new(file_name: './graph_minimum_cut/test_input.txt').run
  end
end
