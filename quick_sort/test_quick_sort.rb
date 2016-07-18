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

  def test_correctness_on_large_input
    running_template(
      input: (1..100_000).to_a.shuffle,
      expect: (1..100_000).to_a
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
    quick_sort = QuickSort.new(input.clone)

    quick_sort.run
    # puts quick_sort.comparison_count

    assert_equal(input.sort, quick_sort.result)
  end

  private

  def running_template(input:, expect:)
    quick_sort = QuickSort.new(input)

    quick_sort.run

    assert_equal(expect, quick_sort.result)
  end
end
