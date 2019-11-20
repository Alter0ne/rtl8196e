delegate int Func ();

class Foo : Object {
	public void bar (MainLoop loop) {
		Object o = new Object ();
		SourceFunc f = () => {
			o = null;
			loop.quit ();
			return false;
		};
		GLib.Idle.add ((owned) f);
	}
}

[CCode (has_target = false)]
delegate void NoTargetFunc ();

int A (int k, Func x1, Func x2, Func x3, Func x4, Func x5) {
	Func B = null;
	B = () => {
		k = k - 1;
		return A (k, B, x1, x2, x3, x4);
	};
	return k <= 0 ? x4 () + x5 () : B ();
}

void B ([CCode (array_length = false, array_null_terminated = true)] int[] array, NoTargetFunc func) {
	Func C = () => { array = null; func (); return 0; };
}

void main () {
	int result = A (10, () => 1, () => -1, () => -1, () => 1, () => 0);
	assert (result == -67);

	var foo = new Foo ();
	var loop = new MainLoop ();
	foo.bar (loop);
	loop.run ();
	assert (foo.ref_count == 1);
}

