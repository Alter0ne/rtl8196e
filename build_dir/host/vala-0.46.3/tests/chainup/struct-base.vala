public struct Foo {
	public int i;
	public int j;
	public Foo () {
		i = 1;
	}
}

public struct Bar : Foo {
	public Bar () {
		base ();
		this.j = 1;
	}
}

void main () {
	var bar = Bar ();
	assert (bar.i == 1);
	assert (bar.j == 1);
}
