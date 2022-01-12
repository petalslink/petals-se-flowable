/**
 * Copyright (c) 2014-2022 Linagora
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
package org.ow2.petals.flowable.utils;

import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.DatatypeConverter;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.ebmwebsourcing.easycommons.stream.EasyByteArrayOutputStream;
import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

public class XslUtils {

    private static final DOMSource EMPTY_DOMSOURCE = new DOMSource(DocumentBuilders.newDocument());

    private XslUtils() {
        // Utility class
    }

    /**
     * Create a XML source from a XSLT style-sheet template and XSL parameters
     * 
     * @param templates
     *            The XSLT style-sheet template to use
     * @param xslParameters
     *            The XSL parameters
     * @param logger
     *            A logger
     * @return The XML source generated
     * @throws TransformerException
     *             An error occurs during trnaformation
     */
    public static Source createXmlPayload(final Templates templates, final Map<QName, String> xslParameters,
            final Logger logger) throws TransformerException {

        // TODO: For performance reasons, check that it is not needed to introduce a pool of transformers to avoid to
        // create a transformer on each request
        final Transformer outputTransformer = templates.newTransformer();

        // We prepare the parameters to set to the XSL transformer:
        if (xslParameters != null) {
            if (logger.isLoggable(Level.FINE)) {
                logger.fine("XSL Parameters:");
                for (final Entry<QName, String> variable : xslParameters.entrySet()) {
                    logger.fine("\t- " + variable.getKey().toString() + " => " + variable.getValue());
                }
            }
            for (final Entry<QName, String> variable : xslParameters.entrySet()) {
                outputTransformer.setParameter(variable.getKey().toString(), variable.getValue());
            }
        }

        final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
        final Result result = new StreamResult(ebaos);
        outputTransformer.transform(EMPTY_DOMSOURCE, result);
        if (logger.isLoggable(Level.FINE)) {
            logger.fine("XML output payload: " + ebaos.toString());
        }
        return new StreamSource(ebaos.toByteArrayInputStream());
    }

    /**
     * Convert a BPMN variable value to a XSL parameter value
     * 
     * @param bpmnVariableValue
     *            A BPMN variable value
     * @return The XSL parameter value
     */
    public static String convertBpmnVariableValueToXslParam(final Object bpmnVariableValue) {

        final String xslParamValue;
        if (bpmnVariableValue != null) {
            if (bpmnVariableValue instanceof Date) {
                final Calendar calendar = Calendar.getInstance();
                calendar.setTime((Date) bpmnVariableValue);
                xslParamValue = DatatypeConverter.printDateTime(calendar);
            } else {
                xslParamValue = bpmnVariableValue.toString();
            }
        } else {
            xslParamValue = "";
        }

        return xslParamValue;
    }

}
