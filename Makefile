release:
	cd frontend; make
	cd backend; make

develop:
	cd frontend; make develop &
	cd backend; make develop

test:
	cd frontend; make test
	cd backend; make test
