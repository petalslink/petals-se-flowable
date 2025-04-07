/**
 * Copyright (c) 2017-2025 Linagora
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
package org.ow2.petals.flowable;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.io.TempDir;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class PlaceholdersProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String PLACEHOLDERS_SU = "placeholders";

    protected static final String PLACEHOLDERS_SU_HOME = "su/placeholders/";

    private static final String PLACEHOLDERS_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/placeholders";

    protected static final QName PLACEHOLDERS_INTERFACE = new QName(PLACEHOLDERS_NAMESPACE, "placeholders");

    protected static final QName PLACEHOLDERS_SERVICE = new QName(PLACEHOLDERS_NAMESPACE, "placeholders-service");

    protected static final String PLACEHOLDERS_ENDPOINT = "edpPlaceholders";

    protected static final QName OPERATION_START = new QName(PLACEHOLDERS_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(PLACEHOLDERS_NAMESPACE, "unlock");

    protected static final QName OPERATION_REUNLOCK = new QName(PLACEHOLDERS_NAMESPACE, "re-unlock");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/placeholders/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "placeholders";

    protected static final String BPMN_USER = "kermit";

    protected static final String USER_TASK_1 = "userTask1";

    protected static final String USER_TASK_2 = "userTask2";

    protected static String COMPONENT_PROPERTIES_FILE;

    @TempDir
    private static Path tempFolder;

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.se_flowable.unit_test.placeholders.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.placeholders.archivageservice.ObjectFactory.class);
        UNMARSHALLER = context.createUnmarshaller();
        MARSHALLER = context.createMarshaller();
        MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    }

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();
        completesComponentUnderTestConfiguration();
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .setParameter(new QName("http://petals.ow2.org/components/extensions/version-5", "properties-file"),
                        new ParameterGenerator() {

                            @Override
                            public String generate() throws Exception {
                                final URL initialComponentPropertiesFile = Thread.currentThread()
                                        .getContextClassLoader()
                                        .getResource("su/placeholders/componentProperties.properties");
                                assertNotNull(initialComponentPropertiesFile, "Component properties file is missing !");

                                final File componentPropertiesFile = Files
                                        .createFile(tempFolder.resolve("component-properties.properties")).toFile();
                                IOUtils.copy(initialComponentPropertiesFile, componentPropertiesFile);

                                COMPONENT_PROPERTIES_FILE = componentPropertiesFile.getAbsolutePath();
                                return COMPONENT_PROPERTIES_FILE;

                            }

                        })
                .registerServiceToDeploy(PLACEHOLDERS_SU, new ServiceConfigurationFactory() {
                    @Override
                    public ServiceConfiguration create() {

                        final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "placeholders.wsdl");
                        assertNotNull(wsdlUrl, "WSDL not found");
                        final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                                PLACEHOLDERS_INTERFACE, PLACEHOLDERS_SERVICE, PLACEHOLDERS_ENDPOINT, wsdlUrl);

                        final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "startResponse.xsl");
                        assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                        serviceConfiguration.addResource(startResponseXslUrl);

                        final URL unlockResponseXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "unlockAckResponse.xsl");
                        assertNotNull(unlockResponseXslUrl, "Output XSL 'unlockAckResponse.xsl' not found");
                        serviceConfiguration.addResource(unlockResponseXslUrl);

                        final URL alreadyUnlockedXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "alreadyUnlocked.xsl");
                        assertNotNull(alreadyUnlockedXslUrl, "Output XSL 'alreadyUnlocked.xsl' not found");
                        serviceConfiguration.addResource(alreadyUnlockedXslUrl);

                        final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "instanceUnknown.xsl");
                        assertNotNull(instanceUnknownXslUrl, "Output XSL 'instanceUnknown.xsl' not found");
                        serviceConfiguration.addResource(instanceUnknownXslUrl);

                        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "placeholders.bpmn");
                        assertNotNull(bpmnUrl, "BPMN file not found");
                        serviceConfiguration.addResource(bpmnUrl);

                        final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(PLACEHOLDERS_SU_HOME + "archivageService.wsdl");
                        assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                        serviceConfiguration.addResource(archivageServiceWsdlUrl);

                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "placeholders.bpmn");
                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                        return serviceConfiguration;
                    }
                }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .postInitComponentUnderTest();
    }
}
