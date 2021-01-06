/**
 * Copyright (c) 2019-2021 Linagora
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
package org.ow2.petals.flowable.incoming.variable;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableValueRequiredException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.ebmwebsourcing.easycommons.json.Xml2JsonConverter;
import com.ebmwebsourcing.easycommons.json.exception.EasyCommonsJsonException;
import com.ebmwebsourcing.easycommons.stream.EasyByteArrayOutputStream;
import com.ebmwebsourcing.easycommons.xml.XMLHelper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import de.odysseus.staxon.json.JsonXMLOutputFactory;

/**
 * Variable implementation for type {@code json}.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class VariableJsonImpl extends VariableImpl {

    public static final String XSL_PLACEHOLDER_PARAM_OUTPUT_NAMESPACE = "http://petals.ow2.org/se/flowable/xsl/param/output/1.0/placeholders";

    private final Xml2JsonConverter xml2JsonConverter;

    private final ObjectMapper jacksonObjectMapper;

    public VariableJsonImpl(final String name, final XPathExpression xpathExprValue, final String type,
            final boolean isRequired, final Templates preXmlTransformation,
            final boolean isPreXmlTranformationResultJson, final QName virtRoot, final Optional<Boolean> multiplePi,
            final Map<String, String> namespaceMappings, final ObjectMapper jacksonObjectMapper, final Logger logger) {
        super(name, xpathExprValue, type, isRequired, logger);
        final XMLOutputFactory jsonOutputFactory = new JsonXMLOutputFactory(
                Xml2JsonConverter.buildConfiguration(virtRoot, multiplePi, namespaceMappings));
        this.xml2JsonConverter = new Xml2JsonConverter(preXmlTransformation, isPreXmlTranformationResultJson, null,
                null, jsonOutputFactory);
        this.jacksonObjectMapper = jacksonObjectMapper;
    }

    /**
     * Return the Flowable variable value in JSON format
     * 
     * @param variableValue
     *            The variable value as {@link Node} to convert in the {@link JsonNode}.
     * @throws VariableException
     *             Invalid value
     */
    private JsonNode convert(final Node variableValue) throws VariableException {

        try (final EasyByteArrayOutputStream osJson = new EasyByteArrayOutputStream()) {
            // We convert XML into JSON using easycommons-json
            this.xml2JsonConverter.transform(osJson, new DOMSource(variableValue), this.logger);

            if (this.logger.isLoggable(Level.FINE)) {
                this.logIntermediateValue("xml -> json", osJson.toString());
            }

            // Next we build a JsonNode from the output stream
            return this.jacksonObjectMapper.readTree(osJson.toByteArrayInputStream());
        } catch (final EasyCommonsJsonException | IOException e) {
            throw new VariableException(this.name, e);
        }
    }

    @Override
    public Object extract(final Document incomingPayload) throws VariableException {
        try {
            final Object variableValueAsObj;
            synchronized (this.xpathExprValue) {
                variableValueAsObj = this.xpathExprValue.evaluate(incomingPayload, XPathConstants.NODE);
            }
            if (variableValueAsObj == null) {
                if (this.isRequired) {
                    throw new VariableValueRequiredException(this.name);
                } else {
                    if (this.logger.isLoggable(Level.FINE)) {
                        this.logEmptyValue();
                    }
                    return null;
                }
            } else {
                final Node variableValueAsNode = (Node) variableValueAsObj;
                if (this.logger.isLoggable(Level.FINE)) {
                    this.logIntermediateValue("xml extraction", XMLHelper.createStringFromDOMNode(variableValueAsNode));
                }

                final JsonNode variableValue = this.convert(variableValueAsNode);
                if (this.logger.isLoggable(Level.FINE)) {
                    this.logFinalValue(variableValue.toString());
                }
                return variableValue;
            }
        } catch (final XPathExpressionException | TransformerException e) {
            throw new VariableException(this.name, e);
        }
    }

}
