/**
 * Copyright (c) 2018-2026 Linagora
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
package org.ow2.petals.samples.flowable.collaboration;

import static org.ow2.petals.flowable.junit.Assert.assertXslTransformation;

import java.io.IOException;

import javax.xml.transform.TransformerException;

import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;

/**
 * Unit tests about XSL transformations embedded into the SU
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class XslTest {

    private static final String XML_RESULT_DIR = "xml-result/";

    private static final String STARTMASTERPROCESS_DIR = "startMasterProcess/";

    private static final String NOMASTERPROCESSWAITINGNOTIFICATION_DIR = "noMasterProcessWaitingNotification/";

    private static final String MASTERPROCESSALREADYNOTIFIED_DIR = "masterProcessAlreadyNotified/";

    private static final String MASTERPROCESSUNKNOWN_DIR = "masterProcessUnknown/";

    private static final String STARTMASTERPROCESS_RESULT_DIR = XML_RESULT_DIR + STARTMASTERPROCESS_DIR;

    private static final String NOMASTERPROCESSWAITINGNOTIFICATION_RESULT_DIR = XML_RESULT_DIR
            + NOMASTERPROCESSWAITINGNOTIFICATION_DIR;

    private static final String MASTERPROCESSALREADYNOTIFIED_RESULT_DIR = XML_RESULT_DIR
            + MASTERPROCESSALREADYNOTIFIED_DIR;

    private static final String MASTERPROCESSUNKNOWN_RESULT_DIR = XML_RESULT_DIR + MASTERPROCESSUNKNOWN_DIR;

    private static final String XSL_STARTMASTERPROCESS = "startMasterProcessResponse.xsl";

    private static final String XSL_NOMASTERPROCESSWAITINGNOTIFICATION = "noMasterProcessWaitingNotification.xsl";

    private static final String XSL_MASTERPROCESSALREADYNOTIFIED = "masterProcessAlreadyNotified.xsl";

    private static final String XSL_MASTERPROCESSUNKNOWN = "masterProcessUnknown.xsl";

    /**
     * Check the XSL 'startMasterProcessResponse.xsl'
     */
    @Test
    public void startMasterProcessResponse_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(STARTMASTERPROCESS_RESULT_DIR + "nominal.xml", XSL_STARTMASTERPROCESS, "AZE123", null,
                null, false);
    }

    /**
     * Check the XSL 'noMasterProcessWaitingNotification.xsl'
     */
    @Test
    public void noMasterProcessWaitingNotification_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(NOMASTERPROCESSWAITINGNOTIFICATION_RESULT_DIR + "nominal.xml",
                XSL_NOMASTERPROCESSWAITINGNOTIFICATION, "AZE123", null, null, true);
    }

    /**
     * Check the XSL 'masterProcessAlreadyNotified.xsl'
     */
    @Test
    public void masterProcessAlreadyNotified_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(MASTERPROCESSALREADYNOTIFIED_RESULT_DIR + "nominal.xml",
                XSL_MASTERPROCESSALREADYNOTIFIED, "AZE123", null, null, true);
    }

    /**
     * Check the XSL 'masterProcessUnknown.xsl'
     */
    @Test
    public void masterProcessUnknown_Nominal() throws IOException, TransformerException, SAXException {
        assertXslTransformation(MASTERPROCESSUNKNOWN_RESULT_DIR + "nominal.xml", XSL_MASTERPROCESSUNKNOWN, "AZE123",
                null, null, true);
    }

}
