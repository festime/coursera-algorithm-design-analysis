require 'minitest/autorun'
require 'minitest/pride'
require_relative './quick_sort'

class TestQuickSort < Minitest::Test
  def test_no_elements
    running_template(input: [], expect: [])
  end

  def test_one_element
    running_template(input: [1], expect: [1])
  end

  def test_correctness_on_simple_input
    running_template(
      input: [3, 4, 5, 1, 2],
      expect: [1, 2, 3, 4, 5]
    )
  end

  def test_correctness_on_sorted_input
    running_template(
      input: (1..7).to_a,
      expect: (1..7).to_a
    )
  end

  def test_correctness_on_reversal_sorted_input
    running_template(
      input: (1..7).to_a.reverse,
      expect: (1..7).to_a
    )
  end

  def test_correctness_on_larger_input
    running_template(
      input: (1..1_000).to_a.shuffle,
      expect: (1..1_000).to_a
    )
  end

  def test_correctness_on_input_has_duplicated_values
    running_template(
      input: [1, 2, 1, 5, 7, 6, 5],
      expect: [1, 1, 2, 5, 5, 6, 7]
    )
  end

  def test_execution_on_test_input_txt
    input = File.readlines('./quick_sort/test_input.txt').map do |line|
      line.strip.to_i
    end

    [:first, :last, :medium].each do |pivot_strategy|
      quick_sort = QuickSort.new(input.clone, pivot_strategy: pivot_strategy)

      quick_sort.run

      assert_equal(input.sort, quick_sort.result)
      puts "pivot_strategy = #{pivot_strategy}"
      puts "comparison_count = #{quick_sort.comparison_count}"
    end
  end

  def test_on_10_test_input_txt
    read_test_input('./quick_sort/10_test_input.txt')

    {
      first: 25,
      last: 29,
      medium: 21
    }.each do |pivot_strategy, expected_comparison_count|
      quick_sort = QuickSort.new(@input.clone, pivot_strategy: pivot_strategy)

      quick_sort.run

      assert_equal(expected_comparison_count, quick_sort.comparison_count)
    end
  end

  def test_on_100_test_input_txt
    read_test_input('./quick_sort/100_test_input.txt')

    {
      first: 615,
      last: 587,
      medium: 518
    }.each do |pivot_strategy, expected_comparison_count|
      quick_sort = QuickSort.new(@input.clone, pivot_strategy: pivot_strategy)

      quick_sort.run

      assert_equal(expected_comparison_count, quick_sort.comparison_count)
    end
  end

  def test_on_1000_test_input_txt
    read_test_input('./quick_sort/1000_test_input.txt')

    {
      first: 10297,
      last: 10184,
      medium: 8921
    }.each do |pivot_strategy, expected_comparison_count|
      quick_sort = QuickSort.new(@input.clone, pivot_strategy: pivot_strategy)

      quick_sort.run

      assert_equal(expected_comparison_count, quick_sort.comparison_count)
    end
  end

  private

  def running_template(input:, expect:)
    [:first, :last, :medium].each do |pivot_strategy|
      quick_sort = QuickSort.new(input, pivot_strategy: pivot_strategy)

      quick_sort.run

      assert_equal(expect, quick_sort.result)
    end
  end

  def read_test_input(file_name)
    @input = File.readlines(file_name).map do |line|
      line.strip.to_i
    end
  end
end
