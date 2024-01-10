/**
 * Copyright (c) 2019-2024 Linagora
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

    private static final String NEW_TRAVEL_DIR = "new-travel/";

    private static final String UNEXPECTED_USER_DIR = "unexpected-user/";

    private static final String TRAVELING_ALREADY_VALIDATED_DIR = "traveling-already-validated/";

    private static final String TRAVELING_REQUEST_UNKNOWN_DIR = "traveling-request-unknown/";

    private static final String VALIDATION_DIR = "validation/";

    private static final String NEW_TRAVEL_RESULT_DIR = XML_RESULT_DIR + NEW_TRAVEL_DIR;

    private static final String UNEXPECTED_USER_RESULT_DIR = XML_RESULT_DIR + UNEXPECTED_USER_DIR;

    private static final String TRAVELING_ALREADY_VALIDATED_RESULT_DIR = XML_RESULT_DIR
            + TRAVELING_ALREADY_VALIDATED_DIR;

    private static final String TRAVELING_REQUEST_UNKNOWN_RESULT_DIR = XML_RESULT_DIR + TRAVELING_REQUEST_UNKNOWN_DIR;

    private static final String VALIDATION_RESULT_DIR = XML_RESULT_DIR + VALIDATION_DIR;

    private static final String XSL_NEW_TRAVELING = "newTravelRequestResponse.xsl";

    private static final String XSL_UNEXPECTED_USER = "unexpectedUser.xsl";

    private static final String XSL_TRAVELING_ALREADY_VALIDATED = "travelingRequestAlreadyValidated.xsl";

    private static final String XSL_TRAVELING_REQUEST_UNKNOWN = "travelingRequestIdUnknown.xsl";

    private static final String XSL_VALIDATION = "validationResponse.xsl";

    /**
     * Check the XSL 'newTravelingRequestResponse.xsl'
     */
    @Test
    public void newTravelingRequestResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(NEW_TRAVEL_RESULT_DIR + "nominal.xml", XSL_NEW_TRAVELING, "AZE123", null, null, false);
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
     * Check the XSL 'travelingRequestAlreadyValidated.xsl'
     */
    @Test
    public void travelingRequestAlreadyValidated_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(TRAVELING_ALREADY_VALIDATED_RESULT_DIR + "nominal.xml", XSL_TRAVELING_ALREADY_VALIDATED,
                "AZE123", null, null, true);
    }

    /**
     * Check the XSL 'travelingRequestIdUnknown.xsl'
     */
    @Test
    public void travelingRequestIdUnknown_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(TRAVELING_REQUEST_UNKNOWN_RESULT_DIR + "nominal.xml", XSL_TRAVELING_REQUEST_UNKNOWN,
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
