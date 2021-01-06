/**
 * Copyright (c) 2016-2021 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.samples.flowable;

import static org.ow2.petals.flowable.junit.Assert.assertXslTransformation;

import java.io.IOException;

import javax.xml.transform.TransformerException;

import org.junit.Test;
import org.xml.sax.SAXException;

/**
 * Unit tests about XSL transformations embedded into the SU
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class XslTest {

    private static final String XML_RESULT_DIR = "xml-result/";

    private static final String ADJUST_DIR = "adjust/";

    private static final String NEW_VACATION_DIR = "new-vacation/";

    private static final String UNEXPECTED_USER_DIR = "unexpected-user/";

    private static final String VACATION_ALREADY_VALIDATED_DIR = "vacation-already-validated/";

    private static final String VACATION_REQUEST_UNKNOWN_DIR = "vacation-request-unknown/";

    private static final String VALIDATION_DIR = "validation/";

    private static final String ADJUST_RESULT_DIR = XML_RESULT_DIR + ADJUST_DIR;

    private static final String NEW_VACATION_RESULT_DIR = XML_RESULT_DIR + NEW_VACATION_DIR;

    private static final String UNEXPECTED_USER_RESULT_DIR = XML_RESULT_DIR + UNEXPECTED_USER_DIR;

    private static final String VACATION_ALREADY_VALIDATED_RESULT_DIR = XML_RESULT_DIR + VACATION_ALREADY_VALIDATED_DIR;

    private static final String VACATION_REQUEST_UNKNOWN_RESULT_DIR = XML_RESULT_DIR + VACATION_REQUEST_UNKNOWN_DIR;

    private static final String VALIDATION_RESULT_DIR = XML_RESULT_DIR + VALIDATION_DIR;

    private static final String XSL_ADJUST = "adjustRequestResponse.xsl";

    private static final String XSL_NEW_VACATION = "newVacationRequestResponse.xsl";

    private static final String XSL_UNEXPECTED_USER = "unexpectedUser.xsl";

    private static final String XSL_VACATION_ALREADY_VALIDATED = "vacationRequestAlreadyValidated.xsl";

    private static final String XSL_VACATION_REQUEST_UNKNOWN = "vacationRequestIdUnknown.xsl";

    private static final String XSL_VALIDATION = "validationResponse.xsl";

    /**
     * Check the XSL 'adjustRequestResponse.xsl'
     */
    @Test
    public void adjustRequestResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(ADJUST_RESULT_DIR + "nominal.xml", XSL_ADJUST, "AZE123", null, null, false);
    }

    /**
     * Check the XSL 'newVacationRequestResponse.xsl'
     */
    @Test
    public void newVacationRequestResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(NEW_VACATION_RESULT_DIR + "nominal.xml", XSL_NEW_VACATION, "AZE123", null, null, false);
    }

    /**
     * Check the XSL 'unexpectedUser.xsl'
     */
    @Test
    public void unexpectedUser_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(UNEXPECTED_USER_RESULT_DIR + "nominal.xml", XSL_UNEXPECTED_USER, "AZE123", "kermit",
                "WXC987", true);
    }

    /**
     * Check the XSL 'vacationRequestAlreadyValidated.xsl'
     */
    @Test
    public void vacationRequestAlreadyValidated_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(VACATION_ALREADY_VALIDATED_RESULT_DIR + "nominal.xml", XSL_VACATION_ALREADY_VALIDATED,
                "AZE123", null, null, true);
    }

    /**
     * Check the XSL 'vacationRequestIdUnknown.xsl'
     */
    @Test
    public void vacationRequestIdUnknown_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(VACATION_REQUEST_UNKNOWN_RESULT_DIR + "nominal.xml", XSL_VACATION_REQUEST_UNKNOWN,
                "AZE123", null, null, true);
    }

    /**
     * Check the XSL 'validationResponse.xsl'
     */
    @Test
    public void validationResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(VALIDATION_RESULT_DIR + "nominal.xml", XSL_VALIDATION, "AZE123", null, null, false);
    }

}
