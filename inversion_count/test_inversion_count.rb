require 'minitest/autorun'
require 'minitest/pride'
require_relative './inversion_count'

class TestInversionCount < Minitest::Test
  def test_completely_sorted_input
    array = (1..100).to_a
    subject = InversionCount.new

    assert_equal(subject.execute(array), array)
    assert_equal(subject.inversion_count, 0)
  end

  def test_simple_example
    array = [1, 2, 5, 4, 3]
    subject = InversionCount.new

    assert_equal(subject.execute(array), array.sort)
    assert_equal(subject.inversion_count, 3)
  end

  def test_large_number_input
    array = (1..99_998).to_a + [100_000, 99_999]
    subject = InversionCount.new

    assert_equal(subject.execute(array), array.sort)
    assert_equal(subject.inversion_count, 1)
  end

  def test_completely_reversal_order
    array = (1..101).to_a.reverse
    subject = InversionCount.new

    assert_equal(subject.execute(array), array.sort)
    assert_equal(subject.inversion_count, 5050)
  end

  def test_only_one_element
    array = [1]
    subject = InversionCount.new

    assert_equal(subject.execute(array), array.sort)
    assert_equal(subject.inversion_count, 0)
  end

  def test_no_elements
    array = []
    subject = InversionCount.new

    assert_equal(subject.execute(array), array.sort)
    assert_equal(subject.inversion_count, 0)
  end
end

# text_input.txt is input data provided by Mooc
#
# in project's root
# enter irb shell and execute following code
# can get the inversion count of test input
#
#
#
# require_relative './inversion_count/inversion_count'
#
# a = File.readlines('./inversion_count/test_input.txt').map do |line|
  # line.strip.to_i
# end
# inversion_count = InversionCount.new
# inversion_count.execute(a)
# inversion_count.inversion_count
