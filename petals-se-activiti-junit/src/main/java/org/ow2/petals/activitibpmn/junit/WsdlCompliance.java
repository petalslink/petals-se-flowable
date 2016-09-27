/**
 * Copyright (c) 2016 Linagora
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
package org.ow2.petals.activitibpmn.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.activiti.bpmn.model.BpmnModel;
import org.ow2.easywsdl.wsdl.api.Description;
import org.ow2.easywsdl.wsdl.api.WSDLException;
import org.ow2.petals.activitibpmn.incoming.operation.EmbeddedProcessDefinition;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.AnnotatedWsdlParser;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.utils.BpmnReader;
import org.ow2.petals.component.framework.api.configuration.SuConfigurationParameters;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.CDKJBIDescriptorBuilder;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.util.ServiceUnitUtil;
import org.ow2.petals.component.framework.util.WSDLUtilImpl;
import org.ow2.petals.jbi.descriptor.JBIDescriptorException;
import org.w3c.dom.Document;

/**
 * All stuff for checking the annotation WSDL compliance with BPMN definition
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class WsdlCompliance {

    private static final Logger LOGGER = Logger.getLogger(WsdlCompliance.class.getName());

    private WsdlCompliance() {
        // Utility class --> no constructor
    }

    /**
     * <p>
     * Assertion checking the WSDL compliance of a service unit.
     * </p>
     * <p>
     * This assertion must be used to check the WSDL annotations of a service unit. So this assertion is expected to be
     * used as unit test assertion of a service unit project. The JBI descriptor of the SU is expected to be the
     * resource 'jbi/jbi.xml'.
     * </p>
     * 
     * @param expectedOperations
     *            List of the expected operations declared in the WSDL.
     */
    public static void assertWsdlCompliance(final QName[] expectedOperations) throws URISyntaxException,
            IOException, JBIDescriptorException, PEtALSCDKException, WSDLException {

        final URL jbiDescriptorUrl = Thread.currentThread().getContextClassLoader().getResource("jbi/jbi.xml");
        assertNotNull("SU JBI descriptor not found", jbiDescriptorUrl);

        final File jbiDescriptorFile = new File(jbiDescriptorUrl.toURI());
        try (final FileInputStream isJbiDescr = new FileInputStream(jbiDescriptorFile)) {
            final Jbi jbiDescriptor = CDKJBIDescriptorBuilder.getInstance().buildJavaJBIDescriptor(isJbiDescr);
            assertNotNull("Invalid JBI descriptor", jbiDescriptor);
            assertNotNull("Invalid JBI descriptor", jbiDescriptor.getServices());
            assertNotNull("Invalid JBI descriptor", jbiDescriptor.getServices().getProvides());
            assertEquals("Invalid JBI descriptor", 1, jbiDescriptor.getServices().getProvides().size());

            final Provides provides = jbiDescriptor.getServices().getProvides().get(0);

            final BpmnReader bpmnReader = new BpmnReader(new SuConfigurationParameters(provides.getAny(), null),
                    jbiDescriptorFile.getParent(), LOGGER);
            final Map<String, EmbeddedProcessDefinition> embeddedBpmnModels = bpmnReader.readBpmnModels();

            final List<BpmnModel> bpmnModels = new ArrayList<>(embeddedBpmnModels.size());
            for (final EmbeddedProcessDefinition embeddedBpmnModel : embeddedBpmnModels.values()) {
                bpmnModels.add(embeddedBpmnModel.getModel());
            }

            final AnnotatedWsdlParser annotatedWdslParser = new AnnotatedWsdlParser(LOGGER);
            final Description wsdlDescription = ServiceUnitUtil.getWsdlDescription(jbiDescriptorFile.getParent(),
                    provides);
            final Document wsdlDocument = WSDLUtilImpl.convertDescriptionToDocument(wsdlDescription);
            final List<AnnotatedOperation> annotatedOperations = annotatedWdslParser.parse(wsdlDocument, bpmnModels,
                    jbiDescriptorFile.getParent());
            // Log all WSDL errors before to assert annotated operations
            if (LOGGER.isLoggable(Level.WARNING)) {
                for (final InvalidAnnotationException encounteredError : annotatedWdslParser.getEncounteredErrors()) {
                    LOGGER.warning(encounteredError.getMessage());
                }
            }
            assertNotNull(annotatedOperations);
            assertEquals(expectedOperations.length, annotatedOperations.size());
            // Assert that all expected operations are in actual operations
            for (final QName expectedOperation : expectedOperations) {
                boolean bFound = false;
                for (final AnnotatedOperation actualOperation : annotatedOperations) {
                    if (actualOperation.getWsdlOperation().equals(expectedOperation)) {
                        bFound = true;
                        break;
                    }
                }
                assertTrue("Operation not found: " + expectedOperation.toString(), bFound);
            }

            // Assert that all actual operation are in expected operations
            for (final AnnotatedOperation actualOperation : annotatedOperations) {
                boolean bFound = false;
                for (final QName expectedOperation : expectedOperations) {
                    if (actualOperation.getWsdlOperation().equals(expectedOperation)) {
                        bFound = true;
                        break;
                    }
                }
                assertTrue("Operation not found: " + actualOperation.toString(), bFound);
            }
        }

    }
}
