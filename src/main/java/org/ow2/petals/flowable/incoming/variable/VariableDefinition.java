/**
 * Copyright (c) 2019-2023 Linagora
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

import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.flowable.bpmn.model.FormProperty;
import org.flowable.bpmn.model.FormValue;
import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatBooleanException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatDateException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatDoubleException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatEnumException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatLongException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableUnsupportedTypeException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableValueRequiredException;
import org.w3c.dom.Document;

import jakarta.xml.bind.DatatypeConverter;

/**
 * Variable definition used as an internal model of a variable.
 *
 * @author Christophe DENEUX - Linagora
 *
 */
public class VariableDefinition {

    private final String name;

    private final XPathExpression xpathExprValue;

    private Templates preXmlTransformation;

    private boolean isPreXmlTranformationResultJson;

    private String type;

    private String datePattern;

    private List<FormValue> enumerations;

    private boolean isRequired;

    private QName virtualRoot;

    private Optional<Boolean> multiplePi;

    private Map<String, String> namespaceMappings;

    public VariableDefinition(final String name, final XPathExpression xpathValue) {
        this(name, xpathValue, null);
    }

    public VariableDefinition(final String name, final XPathExpression xpathValue, final String type) {
        super();
        this.name = name;
        this.xpathExprValue = xpathValue;
        this.type = type;
        this.preXmlTransformation = null;
        this.isPreXmlTranformationResultJson = false;
        this.virtualRoot = null;
        this.multiplePi = Optional.empty();
        this.namespaceMappings = null;
    }

    public void completeDefinition(final FormProperty formProperty) {
        if (this.type == null) {
            this.type = formProperty.getType();
        }

        this.isRequired = formProperty.isRequired();
        if ("enum".equals(this.type)) {
            this.enumerations = formProperty.getFormValues();
            this.datePattern = null;
        } else if ("date".equals(this.type)) {
            this.enumerations = null;
            this.datePattern = formProperty.getDatePattern();
        }
    }

    public String getName() {
        return this.name;
    }

    public String getType() {
        return this.type;
    }

    public XPathExpression getXpathExprValue() {
        return this.xpathExprValue;
    }

    public boolean isRequired() {
        return this.isRequired;
    }

    public String getDatePattern() {
        return this.datePattern;
    }

    public void setDatePattern(final String datePattern) {
        this.datePattern = datePattern;
    }

    public List<FormValue> getEnumerations() {
        return this.enumerations;
    }

    public void setEnumerations(final List<FormValue> enumerations) {
        this.enumerations = enumerations;
    }

    public QName getVirtualRoot() {
        return this.virtualRoot;
    }

    public void setVirtualRoot(final QName virtualRoot) {
        this.virtualRoot = virtualRoot;
    }

    public Optional<Boolean> getMultiplePi() {
        return this.multiplePi;
    }

    public void setMultiplePi(final Optional<Boolean> multiplePi) {
        this.multiplePi = multiplePi;
    }

    public Map<String, String> getNamespaceMappings() {
        return this.namespaceMappings;
    }

    public void setNamespaceMappings(final Map<String, String> namespaceMappings) {
        this.namespaceMappings = namespaceMappings;
    }

    public Templates getPreXmlTransformation() {
        return this.preXmlTransformation;
    }

    public void setPreXmlTransformation(final Templates preXmlTransformation) {
        this.preXmlTransformation = preXmlTransformation;
    }

    public boolean isPreXmlTranformationResultJson() {
        return this.isPreXmlTranformationResultJson;
    }

    public void setPreXmlTranformationResultJson(final boolean isPreXmlTranformationResultJson) {
        this.isPreXmlTranformationResultJson = isPreXmlTranformationResultJson;
    }

    /**
     * Return the Flowable variable value in the right format according to its type
     *
     * @param stringValue
     *            The Flowable varaible value as {@link String} to convert in the right type
     * @throws VariableException
     *             Invalid value
     */
    private Object convert(final String variableValueAsStr) throws VariableException {

        if (this.type.equals("string")) {
            return variableValueAsStr;
        } else if (this.type.equals("long")) {
            try {
                return Long.valueOf(variableValueAsStr);
            } catch (final NumberFormatException e) {
                throw new VariableFormatLongException(this.name, variableValueAsStr);
            }
        } else if (this.type.equals("double")) {
            try {
                return Double.valueOf(variableValueAsStr);
            } catch (final NumberFormatException e) {
                throw new VariableFormatDoubleException(this.name, variableValueAsStr);
            }
        } else if (this.type.equals("enum")) {
            boolean validValue = false;
            for (final FormValue enumeration : this.enumerations) {
                if (variableValueAsStr.equals(enumeration.getId())) {
                    validValue = true;
                }
            }
            if (!validValue) {
                throw new VariableFormatEnumException(this.name, variableValueAsStr);
            } else {
                return variableValueAsStr;
            }
        } else if (this.type.equals("date")) {
            try {
                return DatatypeConverter.parseDateTime(variableValueAsStr).getTime();
            } catch (final IllegalArgumentException e) {
                throw new VariableFormatDateException(this.name, variableValueAsStr, e);
            }
        } else if (this.type.equals("boolean")) {
            if (variableValueAsStr.equalsIgnoreCase("true")
                    || variableValueAsStr.equalsIgnoreCase("false")) {
                return Boolean.valueOf(variableValueAsStr);
            } else {
                throw new VariableFormatBooleanException(this.name, variableValueAsStr);
            }
        } else if (this.type.equals("json")) {
            return Boolean.valueOf(variableValueAsStr);
        } else {
            throw new VariableUnsupportedTypeException(this.name, this.type);
        }
    }

    /**
     *
     * @param incomingPayload
     * @param emptyVariableValueLogger
     *            A callback logger to log message about an empty value extracted
     * @param variableValueLogger
     *            A callback logger to log message about a non-empty value extracted
     * @return The variable value, or {@code null} if empty and not required
     * @throws VariableException
     */
    public Object extract(final Document incomingPayload, final EmptyVariableValueLogger emptyVariableValueLogger,
            final VariableValueLogger variableValueLogger)
            throws VariableException {
        try {
            final String variableValueAsStr;
            synchronized (this.xpathExprValue) {
                variableValueAsStr = this.xpathExprValue.evaluate(incomingPayload);
            }
            if (variableValueAsStr == null || variableValueAsStr.trim().isEmpty()) {
                if (this.isRequired) {
                    throw new VariableValueRequiredException(this.name);
                } else {
                    emptyVariableValueLogger.log(this.name);
                    return null;
                }
            } else {
                variableValueLogger.log(this, variableValueAsStr);
                return this.convert(variableValueAsStr);
            }
        } catch (final XPathExpressionException e) {
            throw new VariableException(this.name, e);
        }
    }

    public interface EmptyVariableValueLogger {
        public void log(final String variableName);
    }

    public interface VariableValueLogger {
        public void log(final VariableDefinition variable, final String variableValue);
    }

}
